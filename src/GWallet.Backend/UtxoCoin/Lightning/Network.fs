namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Net.Sockets
open System.Net
open System.Diagnostics
open System.IO
open System.Text

open FSharp.Core

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Channel
open DotNetLightning.Peer
open DotNetLightning.Chain
open DotNetLightning.Transactions

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks

module Network =

    type Connection =
        internal
            {
                TcpClient: TcpClient
                InitMsg: InitMsg
                Peer: Peer
            }
        member public self.Client = self.TcpClient

    let private hex = DataEncoders.HexEncoder()

    let GetIndexOfDestinationInOutputSeq (dest: IDestination) (outputs: seq<IndexedTxOut>): TxOutIndex =
        let hasRightDestination (output: IndexedTxOut): bool =
            output.TxOut.IsTo dest
        let matchingOutput: IndexedTxOut =
            Seq.find hasRightDestination outputs
        TxOutIndex <| uint16 matchingOutput.N

    let plainInitMsg: InitMsg = {
        Features = Settings.FeatureBits
        TLVStream = [||]
    }

    let private bolt08EncryptedMessageLengthPrefixLength = 18
    // https://github.com/lightningnetwork/lightning-rfc/blob/master/08-transport.md#authenticated-key-exchange-handshake-specification
    let private bolt08ActOneLength = 50
    let private bolt08ActTwoLength = 50
    let private bolt08ActThreeLength = 66

    type StringErrorInner =
        {
            Msg: string
            During: string
        }

    type internal LNInternalError =
    | DNLError of ErrorMsg
    | ConnectError of seq<SocketException>
    | StringError of StringErrorInner
    | DNLChannelError of ChannelError
    | PeerDisconnected of bool
    with
        member this.Message: string =
            match this with
            | StringError { Msg = msg; During = actionAttempted } ->
                SPrintF2 "Error: %s when %s" msg actionAttempted
            | ConnectError errors ->
                let messages = Seq.map (fun (error: SocketException) -> error.Message) errors
                "TCP connection failed: " + (String.concat "; " messages)
            | DNLChannelError error -> "DNL channel error: " + error.Message
            | DNLError error ->
                "Error received from Lightning peer: " +
                match error.Data with
                | [| 01uy |] ->
                    "The number of pending channels exceeds the policy limit." +
                    Environment.NewLine + "Hint: You can try from a new node identity."
                | [| 02uy |] ->
                    "Node is not in sync to latest blockchain blocks." +
                        if Config.BitcoinNet() = Network.RegTest then
                            Environment.NewLine + "Hint: Try mining some blocks before opening."
                        else
                            String.Empty
                | [| 03uy |] ->
                    "Channel capacity too large." + Environment.NewLine + "Hint: Try with a smaller funding amount."
                | _ ->
                    let asciiEncoding = ASCIIEncoding ()
                    "ASCII representation: " + asciiEncoding.GetString error.Data
            | PeerDisconnected whileSendingMsg ->
                if whileSendingMsg then
                    "Error: peer disconnected for unknown reason, abruptly during message transmission"
                else
                    "Error: peer disconnected for unknown reason"

    type LNError internal (error: LNInternalError) =
        member val internal Inner = error with get
        member val Message = error.Message with get

    let internal ReadExactAsync (stream: NetworkStream)
                                (numberBytesToRead: int)
                                    : Async<Result<array<byte>, LNInternalError>> =
        let buf: array<byte> = Array.zeroCreate numberBytesToRead
        let rec read buf totalBytesRead = async {
            let! bytesRead =
                stream.ReadAsync(buf, totalBytesRead, (numberBytesToRead - totalBytesRead))
                |> Async.AwaitTask
            let totalBytesRead = totalBytesRead + bytesRead
            if bytesRead = 0 then
                if totalBytesRead = 0 then
                    return Error (PeerDisconnected false)
                else
                    return Error (PeerDisconnected true)
            else
                if totalBytesRead < numberBytesToRead then
                    return! read buf totalBytesRead
                else
                    return Ok buf
        }
        read buf 0

    let internal ReadAsync (keyRepo: DefaultKeyRepository) (peer: Peer) (stream: NetworkStream)
                               : Async<Result<PeerCommand, LNInternalError>> =
        match peer.ChannelEncryptor.GetNoiseStep() with
        | ActTwo ->
            async {
                let! actTwoRes = ReadExactAsync stream bolt08ActTwoLength
                match actTwoRes with
                | Ok actTwo ->
                    return Ok (ProcessActTwo(actTwo, keyRepo.NodeSecret.PrivateKey))
                | Error err ->
                    return Error err
            }
        | ActThree ->
            async {
                let! actThreeRes = ReadExactAsync stream bolt08ActThreeLength
                match actThreeRes with
                | Ok actThree ->
                    return Ok (ProcessActThree actThree)
                | Error err ->
                    return Error err
            }
        | NoiseComplete ->
            async {
                let! encryptedLengthRes = ReadExactAsync stream bolt08EncryptedMessageLengthPrefixLength
                match encryptedLengthRes with
                | Ok encryptedLength ->
                    let reader length =
                        let buf = Array.zeroCreate length
                        Debug.Assert ((stream.Read (buf, 0, length)) = length, "read length not equal to requested length")
                        buf
                    return Ok (DecodeCipherPacket (encryptedLength, reader))
                | Error err ->
                    return Error err
            }
        | _ ->
            failwith "unreachable"

    type MessageReceived =
    | ChannelMessage of Peer * IChannelMsg
    | OurErrorMessage of Peer * ErrorMsg
    | OtherMessage of Peer

    let ProcessPeerEvents (oldPeer: Peer) (peerEventsResult: Result<List<PeerEvent>, PeerError>): MessageReceived =
        match peerEventsResult with
        | Ok (evt::[]) ->
            match evt with
            | PeerEvent.ReceivedChannelMsg (chanMsg, _) ->
                ChannelMessage (Peer.applyEvent oldPeer evt, chanMsg)
            | PeerEvent.ReceivedError (error, _) ->
                OurErrorMessage (Peer.applyEvent oldPeer evt, error)
            | _ ->
                Infrastructure.LogDebug <| SPrintF1 "Warning: ignoring event that was not ReceivedChannelMsg, it was: %s"
                                                    (evt.GetType().Name)
                OtherMessage <| Peer.applyEvent oldPeer evt
        | Ok _ ->
            failwith "receiving more than one channel event"
        | Error peerError ->
            failwith <| SPrintF1 "couldn't parse chan msg: %s" peerError.Message

    let internal Send (msg: ILightningMsg) (peer: Peer) (stream: NetworkStream): Async<Peer> =
        async {
            let plaintext = msg.ToBytes()
            let ciphertext, newPeerChannelEncryptor = PeerChannelEncryptor.encryptMessage plaintext peer.ChannelEncryptor
            let sentPeer = { peer with ChannelEncryptor = newPeerChannelEncryptor }
            do! stream.WriteAsync(ciphertext, 0, ciphertext.Length) |> Async.AwaitTask
            return sentPeer
        }

    let internal ReportDisconnection (ipEndPoint: IPEndPoint)
                                     (nodeId: NodeId)
                                     (abruptly: bool)
                                     (context: string) =
        let msg =
            SPrintF5
                "peer %s@%s:%i disconnected %s(context: %s)"
                (nodeId.Value.ToString())
                (ipEndPoint.Address.ToString())
                ipEndPoint.Port
                (if abruptly then "after sending partial message " else "")
                context
        Infrastructure.ReportWarningMessage msg
        |> ignore<bool>

    let ConnectAndHandshake (channelEnv: ChannelEnvironment)
                            (channelCounterpartyIP: IPEndPoint)
                                : Async<Result<Connection, LNError>> =
        async {
            let nodeIdForResponder = channelEnv.NodeIdForResponder
            let keyRepo = channelEnv.KeyRepo
            let responderId = channelCounterpartyIP :> EndPoint |> PeerId
            let initialPeer = Peer.CreateOutbound(responderId, nodeIdForResponder, keyRepo.NodeSecret.PrivateKey)
            let act1, peerEncryptor = PeerChannelEncryptor.getActOne initialPeer.ChannelEncryptor
            Debug.Assert((bolt08ActOneLength = act1.Length), "act1 has wrong length")
            let sentAct1Peer = { initialPeer with ChannelEncryptor = peerEncryptor }

            let client = new TcpClient (channelCounterpartyIP.AddressFamily)
            Infrastructure.LogDebug <| SPrintF1 "Connecting over TCP to %A..." channelCounterpartyIP
            let! connectRes = async {
                try
                    do! client.ConnectAsync(channelCounterpartyIP.Address, channelCounterpartyIP.Port) |> Async.AwaitTask
                    return Ok ()
                with
                | ex ->
                    let socketExceptions = FindSingleException<SocketException> ex
                    return Error <| ConnectError socketExceptions
            }
            match connectRes with
            | Error err -> return Error <| LNError err
            | Ok () ->
                let stream = client.GetStream()
                do! stream.WriteAsync(act1, 0, act1.Length) |> Async.AwaitTask

                // Receive act2
                Infrastructure.LogDebug "Receiving Act 2..."
                let! act2Res = ReadAsync keyRepo sentAct1Peer stream
                match act2Res with
                | Error (PeerDisconnected abruptly) ->
                    ReportDisconnection channelCounterpartyIP nodeIdForResponder abruptly "receiving act 2"
                    return Error <| LNError (PeerDisconnected abruptly)
                | Error err -> return Error <| LNError err
                | Ok act2 ->
                    let actThree, receivedAct2Peer =
                        match Peer.executeCommand sentAct1Peer act2 with
                        | Ok (ActTwoProcessed ((actThree, _nodeId), _) as evt::[]) ->
                            let peer = Peer.applyEvent sentAct1Peer evt
                            actThree, peer
                        | Ok _ ->
                            failwith "not one good ActTwoProcessed event"
                        | Error peerError ->
                            failwith <| SPrintF1 "couldn't parse act2: %s" peerError.Message

                    Debug.Assert((bolt08ActThreeLength = actThree.Length), SPrintF1 "act3 has wrong length (not %i)" bolt08ActThreeLength)
                    do! stream.WriteAsync(actThree, 0, actThree.Length) |> Async.AwaitTask

                    let! sentInitPeer = Send plainInitMsg receivedAct2Peer stream

                    // receive init
                    Infrastructure.LogDebug "Receiving init..."
                    let! initRes = ReadAsync keyRepo sentInitPeer stream
                    match initRes with
                    | Error (PeerDisconnected abruptly) ->
                        let context = SPrintF1 "receiving init message while connecting, our init: %A" plainInitMsg
                        ReportDisconnection channelCounterpartyIP nodeIdForResponder abruptly context
                        return Error <| LNError (PeerDisconnected abruptly)
                    | Error err -> return Error <| LNError err
                    | Ok init ->
                        return
                            match Peer.executeCommand sentInitPeer init with
                            | Ok (ReceivedInit (newInit, _) as evt::[]) ->
                                let peer = Peer.applyEvent sentInitPeer evt
                                Ok <| { InitMsg = newInit; Peer = peer; TcpClient = client }
                            | Ok _ ->
                                failwith "not one good ReceivedInit event"
                            | Error peerError ->
                                failwith <| SPrintF1 "couldn't parse init: %s" peerError.Message
        }

    let rec internal ReadUntilChannelMessage (keyRepo: DefaultKeyRepository) (peer: Peer) (stream: NetworkStream)
                                                 : Async<Result<Peer * IChannelMsg, LNInternalError>> =
        async {
            let! channelMsgRes = ReadAsync keyRepo peer stream
            match channelMsgRes with
            | Error err -> return Error err
            | Ok channelMsg ->
                let messageReceived =
                    channelMsg
                    |> Peer.executeCommand peer
                    |> ProcessPeerEvents peer
                match messageReceived with
                | ChannelMessage (newPeer, chanMsg) ->
                    return Ok (newPeer, chanMsg)
                | OurErrorMessage (_, dnlErrorMessage) ->
                    return Error <| DNLError dnlErrorMessage
                | OtherMessage newPeer ->
                    return! ReadUntilChannelMessage keyRepo newPeer stream
        }

    let OpenChannel currency
                    (potentialChannel: PotentialChannel)
                    (channelEnv: ChannelEnvironment)
                    (connection: Connection)
                    (channelCapacity: TransferAmount)
                    (metadata: TransactionMetadata)
                    (passwordPredicate: unit -> string)
                    (balance: decimal)
                        : Async<Result<OutgoingUnfundedChannel, LNError>> =
        let client = connection.Client
        let receivedInit = connection.InitMsg
        let receivedInitPeer = connection.Peer
        let account = channelEnv.Account
        let fundingTxProvider (dest: IDestination, amount: Money, _: FeeRatePerKw) =
            let transferAmount = TransferAmount (amount.ToDecimal MoneyUnit.BTC, balance, currency)
            Debug.Assert (
                             (transferAmount.ValueToSend = channelCapacity.ValueToSend),
                             SPrintF2 "amount passed to fundingTxProvider %A not equal channelCapacity %A"
                                      transferAmount.ValueToSend
                                      channelCapacity.ValueToSend
                         )
            let transactionHex =
                UtxoCoin.Account.SignTransactionForDestination account metadata dest transferAmount (passwordPredicate())
            let network = Account.GetNetwork currency
            let fundingTransaction = Transaction.Load (hex.DecodeData transactionHex, network)
            let outputs = fundingTransaction.Outputs.AsIndexedOutputs ()
            (fundingTransaction |> FinalizedTx, GetIndexOfDestinationInOutputSeq dest outputs) |> Ok

        let fundingAmount = Money (channelCapacity.ValueToSend, MoneyUnit.BTC)
        let nodeIdForResponder = channelEnv.NodeIdForResponder
        let keyRepo = channelEnv.KeyRepo
        let channelKeys, localParams =
            Settings.GetLocalParams true fundingAmount nodeIdForResponder account keyRepo

        async {
            let! feeEstimator = FeeEstimator.Create currency
            let feeEstimator = feeEstimator :> IFeeEstimator
            let initFunder =
                {
                    InputInitFunder.PushMSat = LNMoney.MilliSatoshis 0L
                    TemporaryChannelId = potentialChannel.TemporaryId
                    FundingSatoshis = fundingAmount
                    InitFeeRatePerKw = feeEstimator.GetEstSatPer1000Weight <| ConfirmationTarget.Normal
                    FundingTxFeeRatePerKw = feeEstimator.GetEstSatPer1000Weight <| ConfirmationTarget.Normal
                    LocalParams = localParams
                    RemoteInit = receivedInit
                    ChannelFlags = 0uy
                    ChannelKeys = channelKeys
                }

            let network = Account.GetNetwork currency
            let chanCmd = ChannelCommand.CreateOutbound initFunder
            let stream = client.GetStream()
            let channelCounterpartyIP = client.Client.RemoteEndPoint :?> IPEndPoint
            let initialChan: Channel = ChannelManager.CreateChannel
                                           account
                                           keyRepo
                                           feeEstimator
                                           keyRepo.NodeSecret.PrivateKey
                                           fundingTxProvider
                                           network
                                           nodeIdForResponder

            match Channel.executeCommand initialChan chanCmd with
            | Ok (NewOutboundChannelStarted (openChanMsg, _) as evt::[]) ->
                let sentOpenChan = Channel.applyEvent initialChan evt
                let! sentOpenChanPeer = Send openChanMsg receivedInitPeer stream

                // receive acceptchannel
                Infrastructure.LogDebug "Receiving accept_channel..."
                let! msgRes = ReadUntilChannelMessage keyRepo sentOpenChanPeer stream
                match msgRes with
                | Error (PeerDisconnected abruptly) ->
                    let context = SPrintF1 "receiving accept_channel, our open_channel == %A" openChanMsg
                    ReportDisconnection channelCounterpartyIP nodeIdForResponder abruptly context
                    return Error <| LNError (PeerDisconnected abruptly)
                | Error err -> return Error <| LNError err
                | Ok (receivedOpenChanReplyPeer, chanMsg) ->
                    match chanMsg with
                    | :? AcceptChannelMsg as acceptChannelMsg ->
                        return Ok ({
                                        AcceptChannelMsg = acceptChannelMsg
                                        Channel = sentOpenChan
                                        Peer = receivedOpenChanReplyPeer
                                   })
                    | _ ->
                        return Error <| LNError (StringError {
                            Msg = SPrintF1 "channel message is not accept channel: %s" (chanMsg.GetType().Name)
                            During = "waiting for accept_channel"
                        })
            | Ok evtList ->
                return failwith <| SPrintF1 "event was not a single NewOutboundChannelStarted, it was: %A" evtList
            | Error channelError ->
                return Error <| LNError (DNLChannelError channelError)
        }

    let internal ContinueFromAcceptChannelMsg currency
                                              (keyRepo: DefaultKeyRepository)
                                              (acceptChannelMsg: AcceptChannelMsg)
                                              (sentOpenChan: Channel)
                                              (stream: NetworkStream)
                                              (receivedOpenChanReplyPeer: Peer)
                                                  : Async<Result<string * Channel, LNInternalError>> =
        async {
            match Channel.executeCommand sentOpenChan (ApplyAcceptChannel acceptChannelMsg) with
            | Ok (ChannelEvent.WeAcceptedAcceptChannel(fundingCreated, _) as evt::[]) ->
                let channelCounterpartyIP = receivedOpenChanReplyPeer.PeerId.Value :?> IPEndPoint
                let nodeIdForResponder = receivedOpenChanReplyPeer.TheirNodeId.Value
                let receivedAcceptChannelChan = Channel.applyEvent sentOpenChan evt

                let! sentFundingCreatedPeer = Send fundingCreated receivedOpenChanReplyPeer stream

                Infrastructure.LogDebug "Receiving funding_created..."
                let! msgRes = ReadUntilChannelMessage keyRepo sentFundingCreatedPeer stream
                match msgRes with
                | Error (PeerDisconnected abruptly) ->
                    let context = SPrintF1 "receiving funding_created, their accept_channel == %A" acceptChannelMsg
                    ReportDisconnection channelCounterpartyIP nodeIdForResponder abruptly context
                    return Error <| PeerDisconnected abruptly
                | Error error ->
                    return Error error
                | Ok (_, chanMsg) ->
                    match chanMsg with
                    | :? FundingSignedMsg as fundingSigned ->
                        let chanCmd = ChannelCommand.ApplyFundingSigned fundingSigned
                        let chanEvents = Channel.executeCommand receivedAcceptChannelChan chanCmd
                        match chanEvents with
                        | Ok (ChannelEvent.WeAcceptedFundingSigned (finalizedTx, _) as evt::[]) ->
                            let chan = Channel.applyEvent receivedAcceptChannelChan evt
                            let signedTx: string = finalizedTx.Value.ToHex()
                            let! txId = Account.BroadcastRawTransaction currency signedTx
                            return Ok (txId, chan)
                        | Ok evt ->
                            let msg = SPrintF1 "not one good WeAcceptedFundingSigned chan evt: %s" (evt.GetType().Name)
                            let innerError = { StringErrorInner.Msg = msg; During = "applying their funding_signed message" }
                            return Error <| StringError innerError
                        | Error e ->
                            let msg = SPrintF1 "bad result when expecting WeAcceptedFundingSigned: %s" e.Message
                            let innerError = { Msg = msg; During = "applying their funding_signed message" }
                            return Error <| StringError innerError
                    | _ ->
                        return Error <| StringError {
                            Msg = SPrintF1 "channel message is not funding signed: %s" (chanMsg.GetType().Name)
                            During = "waiting for funding_signed message from counterparty"
                        }

            | Ok evtList ->
                let msg = SPrintF1 "event was not a single WeAcceptedAcceptChannel, it was: %A" evtList
                let innerError = { Msg = msg; During = "applying their accept_channel message" }
                return Error <| StringError innerError
            | Error channelError ->
                match channelError with
                | InvalidAcceptChannel invalidAcceptChannelError ->
                    let msg = SPrintF1 "DNL detected an invalid accept_channel message from fundee: %A"
                                        invalidAcceptChannelError.Errors
                    let innerError = { Msg = msg; During = "applying their accept_channel_message" }
                    return Error <| StringError innerError
                | _ ->
                    let msg = SPrintF1 "unrecognized error from DNL: %A" channelError
                    let innerError = { Msg = msg; During = "applying their accept_channel_message" }
                    return Error <| StringError innerError
            }

    let ContinueFromAcceptChannelAndSave (account: UtxoCoin.NormalUtxoAccount)
                                         (channelCounterpartyIP: IPEndPoint)
                                         (channelDetails: ChannelCreationDetails)
                                             : Async<Result<string, LNError>> = // TxId of Funding Transaction is returned
        async {
            let stream = channelDetails.Client.GetStream()
            let keyRepo = SerializedChannel.UIntToKeyRepo channelDetails.ChannelInfo.KeysSeed
            let! res =
                ContinueFromAcceptChannelMsg (account :> IAccount).Currency
                                             keyRepo
                                             channelDetails.OutgoingUnfundedChannel.AcceptChannelMsg
                                             channelDetails.OutgoingUnfundedChannel.Channel
                                             stream
                                             channelDetails.OutgoingUnfundedChannel.Peer
            match res with
            | Error error -> return Error <| LNError error
            | Ok (fundingTxId, receivedFundingSignedChan) ->
                let fileName = ChannelManager.GetNewChannelFilename()
                SerializedChannel.Save account
                                       receivedFundingSignedChan
                                       channelDetails.ChannelInfo.KeysSeed
                                       channelCounterpartyIP
                                       channelDetails.OutgoingUnfundedChannel.AcceptChannelMsg.MinimumDepth
                                       fileName
                Infrastructure.LogDebug <| SPrintF1 "Channel saved to %s" fileName
                return Ok fundingTxId
        }

    let internal GetFundingLockedMsg (channel: Channel) (channelCommand: ChannelCommand): Channel * FundingLockedMsg =
        let channelEvents = Channel.executeCommand channel channelCommand
        match channelEvents with
        | Ok ((FundingConfirmed _ as evt1)::(WeSentFundingLocked fundingLockedMsg as evt2)::[]) ->
            let channelWithFundingConfirmed = Channel.applyEvent channel evt1
            let channelWithFundingLockedSent = Channel.applyEvent channelWithFundingConfirmed evt2
            channelWithFundingLockedSent, fundingLockedMsg
        | Ok events ->
            failwith <| SPrintF1 "not two good channel events: %A" (List.map (fun evt -> evt.GetType().Name) events)
        | Error e ->
            failwith <| SPrintF1 "bad result when expecting WeSentFundingLocked: %s" e.Message

    let LoadChannelCheckingChannelMessage currency (channelFile: FileInfo): Async<Result<ChannelStatus, LNError>> =
        async {
            let! details = ChannelManager.LoadChannelFetchingDepth currency channelFile
            let txIdHex = details.ChannelId.Value.ToString ()
            let notReestablishedChannel = details.Channel
            let nodeIdForResponder = notReestablishedChannel.RemoteNodeId
            match notReestablishedChannel.State with
            | ChannelState.Normal _ ->
                return Ok (UsableChannel txIdHex)
            | _ ->
                let channelKeysSeed = details.SerializedChannel.KeysRepoSeed
                let channelCounterpartyIP = details.SerializedChannel.CounterpartyIP

                let judgement = ChannelManager.JudgeDepth currency details
                match judgement with
                | NotReady reason ->
                    return Ok (UnusableChannelWithReason (txIdHex, reason))
                | DeepEnough channelCommandAction ->
                    let! channelCommand = channelCommandAction
                    let keyRepo = SerializedChannel.UIntToKeyRepo channelKeysSeed
                    let channelEnvironment: ChannelEnvironment =
                        {
                            Account = details.Account
                            NodeIdForResponder = notReestablishedChannel.RemoteNodeId
                            KeyRepo = keyRepo
                        }
                    let! connectionRes = ConnectAndHandshake channelEnvironment channelCounterpartyIP
                    match connectionRes with
                    | Error err -> return Error err
                    | Ok connection ->
                        let chanCmd =
                            ChannelCommand.CreateChannelReestablish
                        let reestablishMsg, reestablishedChannel =
                            match Channel.executeCommand notReestablishedChannel chanCmd with
                            | Ok (ChannelEvent.WeSentChannelReestablish (ourChannelReestablish) as evt::[]) ->
                                let chan = Channel.applyEvent notReestablishedChannel evt
                                ourChannelReestablish, chan
                            | Ok evtList ->
                                failwith <| SPrintF1 "event was not a single WeSentChannelReestablish, it was: %A" evtList
                            | Error channelError ->
                                failwith <| SPrintF1 "could not execute channel command: %s" channelError.Message

                        let stream = connection.Client.GetStream()
                        Infrastructure.LogDebug "Sending channel_reestablish..."
                        let! sentReestablishPeer = Send reestablishMsg connection.Peer stream
                        let channelWithFundingLockedSent, fundingLocked = GetFundingLockedMsg reestablishedChannel channelCommand
                        let! sentFundingLockedPeer = Send fundingLocked sentReestablishPeer stream
                        Infrastructure.LogDebug "Receiving channel_reestablish or funding_locked..."
                        let! msgRes =
                            ReadUntilChannelMessage  keyRepo sentFundingLockedPeer (connection.Client.GetStream())
                        match msgRes with
                        | Error (PeerDisconnected abruptly) ->
                            ReportDisconnection channelCounterpartyIP nodeIdForResponder abruptly "receiving channel_reestablish or funding_locked"
                            return Error <| LNError (PeerDisconnected abruptly)
                        | Error error -> return Error <| LNError error
                        | Ok (receivedChannelReestablishPeer, chanMsg) ->
                            let! fundingLockedRes =
                                match chanMsg with
                                | :? ChannelReestablishMsg ->
                                    async {
                                        // TODO: validate channel_reestablish
                                        Infrastructure.LogDebug "Received channel_reestablish, now receiving funding_locked..."
                                        let! msgRes =
                                            ReadUntilChannelMessage keyRepo
                                                                    receivedChannelReestablishPeer
                                                                    (connection.Client.GetStream())
                                        match msgRes with
                                        | Error (PeerDisconnected abruptly) ->
                                            ReportDisconnection channelCounterpartyIP nodeIdForResponder abruptly "receiving funding_locked"
                                            return Error (PeerDisconnected abruptly)
                                        | Error errorMsg ->
                                            return Error errorMsg
                                        | Ok (_, chanMsg) ->
                                            match chanMsg with
                                            | :? FundingLockedMsg as fundingLocked ->
                                                // TODO: validate funding_locked
                                                return Ok fundingLocked
                                            | _ ->
                                                let msg = SPrintF1 "channel message is not funding_locked, it is: %s"
                                                                   (chanMsg.GetType().Name)
                                                return Error <| StringError {
                                                    Msg = msg
                                                    During = "waiting for funding_locked"
                                                }
                                    }
                                | :? FundingLockedMsg as fundingLocked ->
                                    // LND can send funding_locked before replying to our channel_reestablish
                                    async {
                                        return Ok fundingLocked
                                    }
                                | _ ->
                                    let msg = SPrintF1 "channel message is not channel_reestablish or funding_locked, instead it is: %s" (chanMsg.GetType().Name)
                                    async {
                                        return Error <| StringError {
                                            Msg = msg
                                            During = "reception of reply to channel_reestablish"
                                        }
                                    }
                            match fundingLockedRes with
                            | Error error -> return Error <| LNError error
                            | Ok fundingLocked ->
                                match Channel.executeCommand channelWithFundingLockedSent (ApplyFundingLocked fundingLocked) with
                                | Ok ((ChannelEvent.BothFundingLocked _) as evt::[]) ->
                                    let bothFundingLockedChan = Channel.applyEvent channelWithFundingLockedSent evt
                                    let serializedChannel = {
                                        details.SerializedChannel with
                                            ChanState = bothFundingLockedChan.State
                                    }
                                    serializedChannel.SaveSerializedChannel channelFile.FullName
                                    Infrastructure.LogDebug <| SPrintF1 "Channel overwritten (with funding transaction locked) at %s"
                                                                        channelFile.FullName
                                    connection.Client.Dispose()
                                    return Ok (UsableChannel txIdHex)
                                | Error channelError ->
                                    return Error <| LNError (DNLChannelError channelError)
                                | Ok (evt::[]) ->
                                    let msg = SPrintF1 "expected event BothFundingLocked, is %s" (evt.GetType().Name)
                                    return Error <| LNError (StringError {
                                        Msg = msg
                                        During = "application of funding_locked"
                                    })
                                | Ok _ ->
                                    let msg = "expected only one event"
                                    return Error <| LNError (StringError {
                                        Msg = msg
                                        During = "application of funding_locked"
                                    })
        }

    let AcceptTheirChannel (account: NormalUtxoAccount)
                               : string*Async<Result<unit, LNError>> =
        let currency = (account :> IAccount).Currency
        let network = Account.GetNetwork currency
        let random = Org.BouncyCastle.Security.SecureRandom () :> Random
        let ip, port = "127.0.0.1", 9735
        let listener = new TcpListener (IPAddress.Parse ip, port)
        listener.Start()
        let channelKeysSeed, keyRepo, temporaryChannelId = ChannelManager.GetSeedAndRepo random
        let ourNodeSecret = keyRepo.NodeSecret.PrivateKey
        let publicKey = ourNodeSecret.PubKey.ToBytes()
        let thisNodeUrl = SPrintF3 "%s@%s:%i" (hex.EncodeData publicKey) ip port
        thisNodeUrl, async {
            use! client = listener.AcceptTcpClientAsync() |> Async.AwaitTask
            let stream = client.GetStream()
            let channelCounterpartyIP = client.Client.RemoteEndPoint :?> IPEndPoint
            // client.Client is actually a Socket (not a TcpClient), LOL
            let peerId = client.Client.RemoteEndPoint |> PeerId
            let initialPeer = Peer.CreateInbound(peerId, ourNodeSecret)

            let! act1Res = ReadExactAsync stream bolt08ActOneLength
            match act1Res with
            | Error err -> return Error <| LNError err
            | Ok act1 ->
                let act1Result = PeerChannelEncryptor.processActOneWithKey act1 ourNodeSecret initialPeer.ChannelEncryptor
                match act1Result with
                | Error err ->
                    return Error <| LNError(StringError {
                        Msg = SPrintF1 "error from DNL: %A" err
                        During = "processing of their act1"
                    })
                | Ok (act2, pce) ->
                    let act2, peerWithSentAct2 =
                        act2, { initialPeer with ChannelEncryptor = pce }
                    do! stream.WriteAsync(act2, 0, act2.Length) |> Async.AwaitTask
                    let! act3Res = ReadExactAsync stream bolt08ActThreeLength
                    match act3Res with
                    | Error err -> return Error <| LNError err
                    | Ok act3 ->
                        let act3Result = PeerChannelEncryptor.processActThree act3 peerWithSentAct2.ChannelEncryptor
                        match act3Result with
                        | Error err ->
                            return Error <| LNError(StringError {
                                Msg = SPrintF1 "error from DNL: %A" err; During = "processing of their act3"
                            })
                        | Ok (remoteNodeId, pce) ->
                            let receivedAct3Peer = { peerWithSentAct2 with ChannelEncryptor = pce }
                            Infrastructure.LogDebug "Receiving init..."
                            let! initRes = ReadAsync keyRepo receivedAct3Peer stream
                            match initRes with
                            | Error (PeerDisconnected abruptly) ->
                                ReportDisconnection channelCounterpartyIP remoteNodeId abruptly "receiving init message while accepting"
                                return Error <| LNError (PeerDisconnected abruptly)
                            | Error err -> return Error <| LNError err
                            | Ok init ->
                                match Peer.executeCommand receivedAct3Peer init with
                                | Error peerError ->
                                    return Error <| LNError(StringError {
                                        Msg = SPrintF1 "couldn't parse init: %s" peerError.Message
                                        During = "receiving init"
                                    })
                                | Ok (ReceivedInit (newInit, _) as evt::[]) ->
                                    let peer = Peer.applyEvent receivedAct3Peer evt
                                    let connection = { InitMsg = newInit; Peer = peer; TcpClient = client }
                                    let! sentInitPeer = Send plainInitMsg connection.Peer stream
                                    Infrastructure.LogDebug "Receiving open_channel..."
                                    let! msgRes = ReadUntilChannelMessage keyRepo sentInitPeer stream
                                    match msgRes with
                                    | Error (PeerDisconnected abruptly) ->
                                        ReportDisconnection channelCounterpartyIP remoteNodeId abruptly "receiving open_channel"
                                        return Error <| LNError (PeerDisconnected abruptly)
                                    | Error error -> return Error <| LNError error
                                    | Ok (receivedOpenChanPeer, chanMsg) ->
                                        match chanMsg with
                                        | :? OpenChannelMsg as openChannelMsg ->
                                            Infrastructure.LogDebug "Creating LocalParams..."
                                            let channelKeys, localParams =
                                                Settings.GetLocalParams false
                                                                        openChannelMsg.FundingSatoshis
                                                                        remoteNodeId
                                                                        account
                                                                        keyRepo
                                            let initFundee: InputInitFundee = {
                                                    TemporaryChannelId = temporaryChannelId
                                                    LocalParams = localParams
                                                    RemoteInit = connection.InitMsg
                                                    ToLocal = LNMoney.MilliSatoshis 0L
                                                    ChannelKeys = channelKeys
                                                }
                                            let chanCmd = ChannelCommand.CreateInbound initFundee
                                            let fundingTxProvider (_: IDestination, _: Money, _: FeeRatePerKw) =
                                                failwith "not funding channel, so unreachable"
                                            Infrastructure.LogDebug "Creating Channel..."
                                            let! feeEstimator = FeeEstimator.Create currency
                                            let initialChan: Channel = ChannelManager.CreateChannel
                                                                           account
                                                                           keyRepo
                                                                           feeEstimator
                                                                           ourNodeSecret
                                                                           fundingTxProvider
                                                                           network
                                                                           remoteNodeId

                                            match Channel.executeCommand initialChan chanCmd with
                                            | Ok (NewInboundChannelStarted _ as evt::[]) ->
                                                let inboundStartedChan =
                                                    Channel.applyEvent initialChan evt

                                                Infrastructure.LogDebug "Applying open_channel..."
                                                let res = Channel.executeCommand inboundStartedChan
                                                                                 (ApplyOpenChannel openChannelMsg)

                                                Infrastructure.LogDebug "Generating accept_channel..."
                                                match res with
                                                | Ok (ChannelEvent.WeAcceptedOpenChannel(acceptChannelMsg, _) as evt::[]) ->
                                                    let receivedOpenChannelChan = Channel.applyEvent inboundStartedChan evt
                                                    Infrastructure.LogDebug "Sending accept_channel..."
                                                    let! sentAcceptChanPeer = Send acceptChannelMsg receivedOpenChanPeer stream

                                                    Infrastructure.LogDebug "Receiving funding_created..."
                                                    let! msgRes =
                                                        ReadUntilChannelMessage keyRepo sentAcceptChanPeer stream
                                                    match msgRes with
                                                    | Error (PeerDisconnected abruptly) ->
                                                        let context = SPrintF2 "receiving funding_created, their open_channel == %A, our accept_channel == %A"
                                                                               openChannelMsg acceptChannelMsg
                                                        ReportDisconnection channelCounterpartyIP remoteNodeId abruptly context
                                                        return Error <| LNError (PeerDisconnected abruptly)
                                                    | Error error -> return Error <| LNError error
                                                    | Ok (receivedFundingCreatedPeer, chanMsg) ->
                                                        match chanMsg with
                                                        | :? FundingCreatedMsg as fundingCreated ->
                                                            match Channel.executeCommand receivedOpenChannelChan (ApplyFundingCreated fundingCreated) with
                                                            | Ok (ChannelEvent.WeAcceptedFundingCreated(fundingSigned, _) as evt::[]) ->
                                                                let receivedFundingCreatedChan =
                                                                    Channel.applyEvent receivedOpenChannelChan evt
                                                                let! _ =
                                                                    Send fundingSigned receivedFundingCreatedPeer stream

                                                                let fileName = ChannelManager.GetNewChannelFilename()
                                                                let remoteIp =
                                                                    client.Client.RemoteEndPoint :?> IPEndPoint
                                                                let endpointToSave =
                                                                    if remoteIp.Address = IPAddress.Loopback then
                                                                        Infrastructure.LogDebug "WARNING: Remote address is the loopback address, saving 127.0.0.2 as IP instead!"
                                                                        IPEndPoint (IPAddress.Parse "127.0.0.2", 9735)
                                                                    else
                                                                        remoteIp
                                                                SerializedChannel.Save account
                                                                                       receivedFundingCreatedChan
                                                                                       channelKeysSeed
                                                                                       endpointToSave
                                                                                       acceptChannelMsg.MinimumDepth
                                                                                       fileName
                                                                Infrastructure.LogDebug <| SPrintF1 "Channel saved to %s"
                                                                                                    fileName

                                                                return Ok ()
                                                            | Ok evtList ->
                                                                return Error <| LNError (StringError {
                                                                    Msg = SPrintF1 "event was not a single WeAcceptedFundingCreated, it was: %A"
                                                                                   evtList
                                                                    During = "application of their funding_created message"
                                                                })
                                                            | Error channelError ->
                                                                return Error <| LNError (StringError {
                                                                    Msg = SPrintF1 "could not apply funding_created: %s"
                                                                                   channelError.Message
                                                                    During = "application of their funding_created message"
                                                                })

                                                        | _ ->
                                                            return Error <| LNError (StringError {
                                                                Msg = SPrintF1 "channel message is not funding_created: %s"
                                                                               (chanMsg.GetType().Name)
                                                                During = "reception of answer to accept_channel"
                                                            })

                                                | Ok evtList ->
                                                    return Error <| LNError (StringError {
                                                        Msg = SPrintF1 "event list was not a single WeAcceptedOpenChannel, it was: %A"
                                                                       evtList
                                                        During = "generation of an accept_channel message"
                                                    })
                                                | Error err ->
                                                    return Error <| LNError (StringError {
                                                        Msg = SPrintF1 "error from DNL: %A" err
                                                        During = "generation of an accept_channel message"
                                                    })

                                            | Ok evtList ->
                                                return Error <| LNError (StringError {
                                                    Msg = SPrintF1 "event was not a single NewInboundChannelStarted, it was: %A"
                                                                   evtList
                                                    During = "execution of CreateChannel command"
                                                })
                                            | Error channelError ->
                                                return Error <| LNError (StringError {
                                                    Msg = SPrintF1 "could not execute channel command: %s"
                                                                   channelError.Message
                                                    During = "execution of CreateChannel command"
                                                })
                                        | _ ->
                                            return Error <| LNError (StringError {
                                                Msg = SPrintF1 "channel message is not open_channel: %s"
                                                               (chanMsg.GetType().Name)
                                                During = "reception of open_channel"
                                            })
                                | Ok _ ->
                                    return Error <| LNError(StringError {
                                        Msg = "not one good ReceivedInit event"
                                        During = "reception of init message"
                                    })
        }
