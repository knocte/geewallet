namespace GWallet.Backend.UtxoCoin.Lightning

open System

open NBitcoin
open DotNetLightning.Channel
open DotNetLightning.Crypto
open DotNetLightning.Transactions
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

    static member internal FundChannel (outgoingUnfundedChannel: OutgoingUnfundedChannel)
                                       (fundingTransaction: Transaction)
                                           : Async<Result<FundedChannel, FundChannelError>> = async {

        let account = outgoingUnfundedChannel.Account
        let currency = (account :> IAccount).Currency
        let peerNode = outgoingUnfundedChannel.PeerNode
        let channelWaitingForFundingTx = outgoingUnfundedChannel.ChannelWaitingForFundingTx

        let channelWaitingForFundingSignedRes =
            let fundingOutputIndex =
                let indexedOutputs = fundingTransaction.Outputs.AsIndexedOutputs()
                let hasRightDestination (indexedOutput: IndexedTxOut): bool =
                    indexedOutput.TxOut.IsTo outgoingUnfundedChannel.FundingDestination
                let matchingOutput: IndexedTxOut =
                    Seq.find hasRightDestination indexedOutputs
                TxOutIndex <| uint16 matchingOutput.N
            let finalizedFundingTransaction = FinalizedTx fundingTransaction
            channelWaitingForFundingTx.CreateFundingTx
                finalizedFundingTransaction
                fundingOutputIndex
        match channelWaitingForFundingSignedRes with
        | Error err ->
            return failwith <| SPrintF1 "DNL rejected our funding tx: %s" (err.ToString())
        | Ok (fundingCreatedMsg, channelWaitingForFundingSigned) ->
            let! peerNodeAfterFundingCreated =
                peerNode.SendMsg fundingCreatedMsg
            let! recvChannelMsgRes = peerNodeAfterFundingCreated.RecvChannelMsg()
            match recvChannelMsgRes with
            | Error (RecvMsg recvMsgRes) -> return Error <| RecvFundingSigned recvMsgRes
            | Error (ReceivedPeerErrorMessage (peerNodeAfterFundingSigned, errorMessage)) ->
                return Error <| FundingCreatedPeerErrorResponse
                    (peerNodeAfterFundingSigned, errorMessage)
            | Ok (peerNodeAfterFundingSigned, channelMsg) ->
                match channelMsg with
                | :? FundingSignedMsg as fundingSignedMsg ->
                    let channelRes =
                        channelWaitingForFundingSigned.ApplyFundingSigned fundingSignedMsg
                    match channelRes with
                    | Error err ->
                        let! peerNodeAfterError =
                            peerNodeAfterFundingSigned.SendError
                                err.Message
                                (Some channelWaitingForFundingSigned.ChannelId)
                        return Error <| InvalidFundingSigned
                            (peerNodeAfterError, err)
                    | Ok (finalizedTx, channel) ->
                        let connectedChannel = {
                            PeerNode = peerNodeAfterFundingSigned
                            Channel = { Channel = channel }
                            Account = account
                            MinimumDepth = outgoingUnfundedChannel.MinimumDepth
                            ChannelIndex = outgoingUnfundedChannel.ChannelIndex
                        }
                        connectedChannel.SaveToWallet()
                        let! _txid =
                            let signedTx: string = finalizedTx.Value.ToHex()
                            Account.BroadcastRawTransaction currency signedTx
                        let fundedChannel = {
                            ConnectedChannel = connectedChannel
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
                let! channelWaitingForFundingCreatedRes = async {
                    let defaultFinalScriptPubKey = ScriptManager.CreatePayoutScript account
                    let currency = (account :> IAccount).Currency
                    let! channelOptions = MonoHopUnidirectionalChannel.DefaultChannelOptions (currency)
                    let network = UtxoCoin.Account.GetNetwork currency
                    let localParams =
                        let funding = openChannelMsg.FundingSatoshis
                        Settings.GetLocalParams funding
                    let fundingTxMinimumDepth =
                        MonoHopUnidirectionalChannel.DefaultFundingTxMinimumDepth
                    return Channel.NewInbound(
                        Settings.PeerLimits,
                        channelOptions,
                        false,
                        nodeMasterPrivKey,
                        channelIndex,
                        network,
                        nodeId,
                        fundingTxMinimumDepth,
                        Some defaultFinalScriptPubKey,
                        openChannelMsg,
                        localParams,
                        peerNodeAfterOpenChannel.InitMsg
                    )
                }
                match channelWaitingForFundingCreatedRes with
                | Error err ->
                    let! peerNodeAfterErrorSent =
                        peerNodeAfterOpenChannel.SendError
                            err.Message
                            (Some openChannelMsg.TemporaryChannelId)
                    return Error <| InvalidOpenChannel
                        (peerNodeAfterErrorSent, err)
                | Ok (acceptChannelMsg, channelWaitingForFundingCreated) ->
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
                            let channelRes =
                                channelWaitingForFundingCreated.ApplyFundingCreated
                                    fundingCreatedMsg
                            match channelRes with
                            | Error err ->
                                let! peerNodeAfterErrorSent =
                                    peerNodeAfterFundingCreated.SendError
                                        err.Message
                                        (Some openChannelMsg.TemporaryChannelId)
                                return Error <| InvalidFundingCreated
                                    (peerNodeAfterErrorSent, err)
                            | Ok (fundingSignedMsg, channel) ->
                                let minimumDepth = acceptChannelMsg.MinimumDepth
                                let connectedChannelAfterFundingCreated = {
                                    PeerNode = peerNodeAfterFundingCreated
                                    Channel = { Channel = channel }
                                    Account = account
                                    MinimumDepth = minimumDepth
                                    ChannelIndex = channelIndex
                                }
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
            self.ConnectedChannel.FundingScriptCoin

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


