namespace GWallet.Backend.UtxoCoin.Lightning

open System

open NBitcoin
open DotNetLightning.Channel
open DotNetLightning.Utils
open DotNetLightning.Serialization.Msgs
open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks
open GWallet.Backend.UtxoCoin

type internal FundChannelError =
    | RecvFundingSigned of RecvMsgError
    | FundingCreatedPeerErrorResponse of PeerNode * PeerErrorMessage
    | InvalidFundingSigned of PeerNode * ChannelError
    | ExpectedFundingSigned of ILightningMsg
    interface IErrorMsg with
        member self.Message =
            match self with
            | RecvFundingSigned err ->
                SPrintF1 "Error receiving funding_signed message: %s" (err :> IErrorMsg).Message
            | FundingCreatedPeerErrorResponse (_, err) ->
                SPrintF1 "Peer responded to our funding_created message with an error: %s" (err :> IErrorMsg).Message
            | InvalidFundingSigned (_, err) ->
                SPrintF1 "Invalid funding_signed message: %s" err.Message
            | ExpectedFundingSigned msg ->
                SPrintF1 "Expected funding_signed msg, got %A" (msg.GetType())
        member __.ChannelBreakdown: bool =
            false

    member internal self.PossibleBug =
        match self with
        | RecvFundingSigned err -> err.PossibleBug
        | FundingCreatedPeerErrorResponse _
        | InvalidFundingSigned _
        | ExpectedFundingSigned _ -> false

type internal AcceptChannelError =
    | RecvOpenChannel of RecvMsgError
    | PeerErrorMessageInsteadOfOpenChannel of PeerNode * PeerErrorMessage
    | InvalidOpenChannel of PeerNode * ChannelError
    | ExpectedOpenChannel of ILightningMsg
    | RecvFundingCreated of RecvMsgError
    | AcceptChannelPeerErrorResponse of PeerNode * PeerErrorMessage
    | InvalidFundingCreated of PeerNode * ChannelError
    | ExpectedFundingCreated of ILightningMsg
    interface IErrorMsg with
        member self.Message =
            match self with
            | RecvOpenChannel err ->
                SPrintF1 "Error receiving open_channel msg: %s" (err :> IErrorMsg).Message
            | PeerErrorMessageInsteadOfOpenChannel (_, err) ->
                SPrintF1 "Peer sent an error message instead of open_channel: %s" (err :> IErrorMsg).Message
            | InvalidOpenChannel (_, err) ->
                SPrintF1 "Invalid open_channel message: %s" err.Message
            | ExpectedOpenChannel msg ->
                SPrintF1 "Expected open_channel msg, got %A" (msg.GetType())
            | RecvFundingCreated err ->
                SPrintF1 "Error receiving funding_created message: %s" (err :> IErrorMsg).Message
            | AcceptChannelPeerErrorResponse (_, err) ->
                SPrintF1 "Peer responded to our accept_channel message with an error: %s" (err :> IErrorMsg).Message
            | InvalidFundingCreated (_, err) ->
                SPrintF1 "Invalid funding_created message: %s" err.Message
            | ExpectedFundingCreated msg ->
                SPrintF1 "Expected funding_created message, got %A" (msg.GetType())
        member __.ChannelBreakdown: bool =
            false

    member internal self.PossibleBug =
        match self with
        | RecvOpenChannel err -> err.PossibleBug
        | RecvFundingCreated err -> err.PossibleBug
        | PeerErrorMessageInsteadOfOpenChannel _
        | InvalidOpenChannel _
        | ExpectedOpenChannel _
        | AcceptChannelPeerErrorResponse _
        | InvalidFundingCreated _
        | ExpectedFundingCreated _ -> false

type internal FundedChannel =
    {
        ConnectedChannel: ConnectedChannel
        TheirFundingLockedMsgOpt: Option<FundingLockedMsg>
    }
    interface IDisposable with
        member self.Dispose() =
            (self.ConnectedChannel :> IDisposable).Dispose()

    static member FundChannel (outgoingUnfundedChannel: OutgoingUnfundedChannel)
                                  : Async<Result<FundedChannel, FundChannelError>> = async {

        let connectedChannel = outgoingUnfundedChannel.ConnectedChannel
        let currency = (connectedChannel.Account :> IAccount).Currency
        let peerNode = connectedChannel.PeerNode
        let channel = connectedChannel.Channel

        let! peerNodeAfterFundingCreated =
            peerNode.SendMsg outgoingUnfundedChannel.FundingCreatedMsg
        let! recvChannelMsgRes = peerNodeAfterFundingCreated.RecvChannelMsg()
        match recvChannelMsgRes with
        | Error (RecvMsg recvMsgRes) -> return Error <| RecvFundingSigned recvMsgRes
        | Error (ReceivedPeerErrorMessage (peerNodeAfterFundingSigned, errorMessage)) ->
            return Error <| FundingCreatedPeerErrorResponse
                (peerNodeAfterFundingSigned, errorMessage)
        | Ok (peerNodeAfterFundingSigned, channelMsg) ->
            match channelMsg with
            | :? FundingSignedMsg as fundingSignedMsg ->
                let finalizedTxRes, channelAfterFundingSigned =
                    let channelCmd = ChannelCommand.ApplyFundingSigned fundingSignedMsg
                    channel.ExecuteCommand channelCmd <| function
                        | (WeAcceptedFundingSigned(finalizedTx, _)::[]) -> Some finalizedTx
                        | _ -> None
                let connectedChannelAfterFundingSigned = {
                    connectedChannel with
                        PeerNode = peerNodeAfterFundingSigned
                        Channel = channelAfterFundingSigned
                }
                match finalizedTxRes with
                | Error err ->
                    let! connectedChannelAfterError =
                        connectedChannelAfterFundingSigned.SendError err.Message
                    return Error <| InvalidFundingSigned
                        (connectedChannelAfterError.PeerNode, err)
                | Ok finalizedTx ->
                    connectedChannelAfterFundingSigned.SaveToWallet()
                    let! _txid =
                        let signedTx: string = finalizedTx.Value.ToHex()
                        Account.BroadcastRawTransaction currency signedTx
                    let fundedChannel = {
                        ConnectedChannel = connectedChannelAfterFundingSigned
                        TheirFundingLockedMsgOpt = None
                    }
                    return Ok fundedChannel
            | _ -> return Error <| ExpectedFundingSigned channelMsg
    }

    static member internal AcceptChannel (peerNode: PeerNode)
                                         (account: NormalUtxoAccount)
                                             : Async<Result<FundedChannel, AcceptChannelError>> = async {
        let channelIndex =
            let random = Org.BouncyCastle.Security.SecureRandom() :> Random
            random.Next(1, Int32.MaxValue / 2)
        let nodeId = peerNode.RemoteNodeId
        let! recvChannelMsgRes = peerNode.RecvChannelMsg()
        match recvChannelMsgRes with
        | Error (RecvMsg recvMsgError) -> return Error <| RecvOpenChannel recvMsgError
        | Error (ReceivedPeerErrorMessage (peerNodeAfterOpenChannel, errorMessage)) ->
            return Error <| PeerErrorMessageInsteadOfOpenChannel
                (peerNodeAfterOpenChannel, errorMessage)
        | Ok (peerNodeAfterOpenChannel, channelMsg) ->
            match channelMsg with
            | :? OpenChannelMsg as openChannelMsg ->
                let nodeMasterPrivKey = peerNode.NodeMasterPrivKey ()
                let! channel =
                    let fundingTxProvider (_: IDestination, _: Money, _: FeeRatePerKw) =
                        failwith "not funding channel, so unreachable"
                    MonoHopUnidirectionalChannel.Create
                        nodeId
                        account
                        nodeMasterPrivKey
                        channelIndex
                        fundingTxProvider
                        WaitForInitInternal
                let channelPrivKeys = channel.ChannelPrivKeys
                let localParams =
                    let funding = openChannelMsg.FundingSatoshis
                    let defaultFinalScriptPubKey = ScriptManager.CreatePayoutScript account
                    let currency = (account:>IAccount).Currency
                    channel.LocalParams funding currency defaultFinalScriptPubKey false
                let res, channelAfterOpenChannel =
                    let channelCmd =
                        let inputInitFundee: InputInitFundee = {
                            TemporaryChannelId = openChannelMsg.TemporaryChannelId
                            LocalParams = localParams
                            RemoteInit = peerNodeAfterOpenChannel.InitMsg
                            ToLocal = LNMoney 0L
                            ChannelPrivKeys = channelPrivKeys
                        }
                        ChannelCommand.CreateInbound inputInitFundee
                    channel.ExecuteCommand channelCmd <| function
                        | (NewInboundChannelStarted _)::[] -> Some ()
                        | _ -> None
                UnwrapResult res "error executing create inbound channel command"

                let acceptChannelMsgRes, channelAfterAcceptChannel =
                    let channelCmd = ApplyOpenChannel openChannelMsg
                    channelAfterOpenChannel.ExecuteCommand channelCmd <| function
                        | WeAcceptedOpenChannel(acceptChannelMsg, _)::[] -> Some acceptChannelMsg
                        | _ -> None
                match acceptChannelMsgRes with
                | Error err ->
                    let connectedChannel = {
                        PeerNode = peerNodeAfterOpenChannel
                        Channel = channelAfterAcceptChannel
                        Account = account
                        MinimumDepth = BlockHeightOffset32 0u
                        ChannelIndex = channelIndex
                    }
                    let! connectedChannelAfterErrorSent = connectedChannel.SendError err.Message
                    return Error <| InvalidOpenChannel
                        (connectedChannelAfterErrorSent.PeerNode, err)
                | Ok acceptChannelMsg ->
                    let! peerNodeAfterAcceptChannel = peerNodeAfterOpenChannel.SendMsg acceptChannelMsg
                    let! recvChannelMsgRes = peerNodeAfterAcceptChannel.RecvChannelMsg()
                    match recvChannelMsgRes with
                    | Error (RecvMsg recvMsgError) ->
                        return Error <| RecvFundingCreated recvMsgError
                    | Error (ReceivedPeerErrorMessage (peerNodeAfterFundingCreated, errorMessage)) ->
                        return Error <| AcceptChannelPeerErrorResponse
                            (peerNodeAfterFundingCreated, errorMessage)
                    | Ok (peerNodeAfterFundingCreated, channelMsg) ->
                        match channelMsg with
                        | :? FundingCreatedMsg as fundingCreatedMsg ->
                            let fundingSignedMsgRes, channelAfterFundingCreated =
                                let channelCmd = ApplyFundingCreated fundingCreatedMsg
                                channelAfterAcceptChannel.ExecuteCommand channelCmd <| function
                                    | WeAcceptedFundingCreated(fundingSignedMsg, _)::[] -> Some fundingSignedMsg
                                    | _ -> None
                            let minimumDepth = acceptChannelMsg.MinimumDepth
                            let connectedChannelAfterFundingCreated = {
                                PeerNode = peerNodeAfterFundingCreated
                                Channel = channelAfterFundingCreated
                                Account = account
                                MinimumDepth = minimumDepth
                                ChannelIndex = channelIndex
                            }
                            match fundingSignedMsgRes with
                            | Error err ->
                                let! connectedChannelAfterErrorSent =
                                    connectedChannelAfterFundingCreated.SendError err.Message
                                return Error <| InvalidFundingCreated
                                    (connectedChannelAfterErrorSent.PeerNode, err)
                            | Ok fundingSignedMsg ->
                                connectedChannelAfterFundingCreated.SaveToWallet()

                                let! peerNodeAfterFundingSigned =
                                    connectedChannelAfterFundingCreated.PeerNode.SendMsg fundingSignedMsg
                                let connectedChannelAfterFundingSigned =
                                    { connectedChannelAfterFundingCreated with
                                          PeerNode = peerNodeAfterFundingSigned }

                                let fundedChannel = {
                                    ConnectedChannel = connectedChannelAfterFundingSigned
                                    TheirFundingLockedMsgOpt = None
                                }
                                return Ok fundedChannel
                        | _ -> return Error <| ExpectedFundingCreated channelMsg
            | _ -> return Error <| ExpectedOpenChannel channelMsg
    }

    member internal self.FundingScriptCoin
        with get(): ScriptCoin =
            UnwrapOption
                self.ConnectedChannel.FundingScriptCoin
                "The FundedChannel type is created by funding a channel and \
                guarantees that the underlying ChannelState has a script coin"

    member internal self.GetConfirmations(): Async<BlockHeightOffset32> = async {
        let currency = (self.ConnectedChannel.Account :> IAccount).Currency
        let! confirmationCount =
            let txId = self.FundingTxId.ToString()
            Server.Query currency
                         (QuerySettings.Default ServerSelectionMode.Fast)
                         (ElectrumClient.GetConfirmations txId)
                         None
        return confirmationCount |> BlockHeightOffset32
    }

    member internal self.GetLocationOnChain(): Async<Option<BlockHeight * TxIndexInBlock>> = async {
        let currency = (self.ConnectedChannel.Account :> IAccount).Currency
        let fundingScriptCoin = self.FundingScriptCoin
        let txIdHex: string = self.ConnectedChannel.FundingTxId.ToString()
        let fundingDestination: TxDestination = fundingScriptCoin.ScriptPubKey.GetDestination()
        let network = UtxoCoin.Account.GetNetwork currency
        let fundingAddress: BitcoinAddress = fundingDestination.GetAddress network
        let fundingAddressString: string = fundingAddress.ToString()
        let scriptHash = Account.GetElectrumScriptHashFromPublicAddress currency fundingAddressString
        let! historyList =
            Server.Query currency
                         (QuerySettings.Default ServerSelectionMode.Fast)
                         (ElectrumClient.GetBlockchainScriptHashHistory scriptHash)
                         None
        if Seq.isEmpty historyList then
            return None
        else
            let history = Seq.head historyList
            let fundingBlockHeight = BlockHeight history.Height
            let! merkleResult =
                Server.Query currency
                             (QuerySettings.Default ServerSelectionMode.Fast)
                             (ElectrumClient.GetBlockchainScriptHashMerkle txIdHex history.Height)
                             None
            let fundingTxIndexInBlock = TxIndexInBlock merkleResult.Pos
            return Some(fundingBlockHeight, fundingTxIndexInBlock)
    }

    member self.FundingTxId
        with get(): TransactionIdentifier = self.ConnectedChannel.FundingTxId

    member internal self.MinimumDepth
        with get(): BlockHeightOffset32 = self.ConnectedChannel.MinimumDepth

    member self.ChannelId
        with get(): ChannelIdentifier = self.ConnectedChannel.ChannelId


