namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Diagnostics

open NBitcoin
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Chain
open DotNetLightning.Channel
open DotNetLightning.Transactions
open DotNetLightning.Utils
open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.FSharpUtil.UwpHacks
open GWallet.Backend.UtxoCoin

type internal OpenChannelError =
    | InvalidChannelParameters of PeerNode * ChannelError
    | RecvAcceptChannel of RecvMsgError
    | OpenChannelPeerErrorResponse of PeerNode * PeerErrorMessage
    | ExpectedAcceptChannel of ILightningMsg
    | InvalidAcceptChannel of PeerNode * ChannelError
    interface IErrorMsg with
        member self.Message =
            match self with
            | InvalidChannelParameters (_, err) ->
                SPrintF1 "Invalid channel parameters: %s" err.Message
            | RecvAcceptChannel err ->
                SPrintF1 "Error receiving accept_channel: %s" (err :> IErrorMsg).Message
            | OpenChannelPeerErrorResponse (_, err) ->
                SPrintF1 "Peer responded to our open_channel with an error message: %s" (err :> IErrorMsg).Message
            | ExpectedAcceptChannel msg ->
                SPrintF1 "Expected accept_channel, got %A" (msg.GetType())
            | InvalidAcceptChannel (_, err) ->
                SPrintF1 "Invalid accept_channel message: %s" err.Message
    member internal self.PossibleBug =
        match self with
        | RecvAcceptChannel err -> err.PossibleBug
        | InvalidChannelParameters _
        | OpenChannelPeerErrorResponse _
        | ExpectedAcceptChannel _
        | InvalidAcceptChannel _ -> false

type internal OutgoingUnfundedChannel =
    {
        ConnectedChannel: ConnectedChannel
        FundingCreatedMsg: FundingCreatedMsg
    }
    static member OpenChannel (peerNode: PeerNode)
                              (account: NormalUtxoAccount)
                              (channelCapacity: TransferAmount)
                              (metadata: TransactionMetadata)
                              (password: string)
                                  : Async<Result<OutgoingUnfundedChannel, OpenChannelError>> = async {
        let hex = DataEncoders.HexEncoder()

        let network = Account.GetNetwork (account:>IAccount).Currency
        let nodeId = peerNode.RemoteNodeId
        let nodeSecret = peerNode.NodeSecret
        let channelIndex =
            let random = Org.BouncyCastle.Security.SecureRandom() :> Random
            random.Next(1, Int32.MaxValue / 2)
        let! channel =
            let fundingTxProvider (dest: IDestination, amount: Money, _feeRate: FeeRatePerKw) =
                Debug.Assert(amount.ToDecimal MoneyUnit.BTC = channelCapacity.ValueToSend)
                let transactionHex =
                    UtxoCoin.Account.SignTransactionForDestination
                        account
                        metadata
                        dest
                        channelCapacity
                        password
                let fundingTransaction = Transaction.Load (hex.DecodeData transactionHex, network)
                let fundingOutputIndex =
                    let indexedOutputs = fundingTransaction.Outputs.AsIndexedOutputs()
                    let hasRightDestination (indexedOutput: IndexedTxOut): bool =
                        indexedOutput.TxOut.IsTo dest
                    let matchingOutput: IndexedTxOut =
                        Seq.find hasRightDestination indexedOutputs
                    TxOutIndex <| uint16 matchingOutput.N
                let finalizedFundingTransaction = FinalizedTx fundingTransaction
                Ok (finalizedFundingTransaction, fundingOutputIndex)
            MonoHopUnidirectionalChannel.Create
                nodeId
                account
                nodeSecret
                channelIndex
                fundingTxProvider
                WaitForInitInternal
        let localParams =
            let funding = Money(channelCapacity.ValueToSend, MoneyUnit.BTC)
            let defaultFinalScriptPubKey = ScriptManager.CreatePayoutScript account
            channel.LocalParams funding defaultFinalScriptPubKey true
        let temporaryChannelId = ChannelIdentifier.NewRandom()
        let feeRate =
            channel.Channel.FeeEstimator.GetEstSatPer1000Weight ConfirmationTarget.Normal
        let openChannelMsgRes, channelAfterOpenChannel =
            let channelCommand =
                let inputInitFunder = {
                    InputInitFunder.PushMSat = LNMoney 0L
                    TemporaryChannelId = temporaryChannelId.DnlChannelId
                    FundingSatoshis = Money (channelCapacity.ValueToSend, MoneyUnit.BTC)
                    InitFeeRatePerKw = feeRate
                    FundingTxFeeRatePerKw = feeRate
                    LocalParams = localParams
                    RemoteInit = peerNode.InitMsg
                    ChannelFlags = 0uy
                    ChannelKeys = channel.ChannelKeys
                }
                ChannelCommand.CreateOutbound inputInitFunder
            channel.ExecuteCommand channelCommand <| function
                | NewOutboundChannelStarted(openChannelMsg, _)::[] -> Some openChannelMsg
                | _ -> None
        match openChannelMsgRes with
        | Error channelError ->
            return Error <| InvalidChannelParameters (peerNode, channelError)
        | Ok openChannelMsg ->
            let! peerNodeAfterOpenChannel = peerNode.SendMsg openChannelMsg

            Infrastructure.LogDebug "Receiving accept_channel..."
            let! recvChannelMsgRes = peerNodeAfterOpenChannel.RecvChannelMsg()
            match recvChannelMsgRes with
            | Error (RecvMsg recvMsgError) -> return Error <| RecvAcceptChannel recvMsgError
            | Error (ReceivedPeerErrorMessage (peerNodeAfterAcceptChannel, errorMessage)) ->
                (peerNodeAfterAcceptChannel :> IDisposable).Dispose()
                return Error <| OpenChannelPeerErrorResponse
                    (peerNodeAfterAcceptChannel, errorMessage)
            | Ok (peerNodeAfterAcceptChannel, channelMsg) ->
                match channelMsg with
                | :? AcceptChannelMsg as acceptChannelMsg ->
                    let fundingCreatedMsgRes, channelAfterFundingCreated =
                        let channelCmd = ApplyAcceptChannel acceptChannelMsg
                        channelAfterOpenChannel.ExecuteCommand channelCmd <| function
                            | (WeAcceptedAcceptChannel(fundingCreatedMsg, _)::[])
                                -> Some fundingCreatedMsg
                            | _ -> None
                    match fundingCreatedMsgRes with
                    | Error err ->
                        return Error <| InvalidAcceptChannel
                            (peerNodeAfterAcceptChannel, err)
                    | Ok fundingCreatedMsg ->
                        let minimumDepth = acceptChannelMsg.MinimumDepth
                        let connectedChannel = {
                            PeerNode = peerNodeAfterAcceptChannel
                            Channel = channelAfterFundingCreated
                            Account = account
                            // TODO: move this into FundedChannel?
                            MinimumDepth = minimumDepth
                            ChannelIndex = channelIndex
                        }
                        let outgoingUnfundedChannel = {
                            ConnectedChannel = connectedChannel
                            FundingCreatedMsg = fundingCreatedMsg
                        }
                        return Ok outgoingUnfundedChannel
                | _ -> return Error <| ExpectedAcceptChannel channelMsg
    }

    member internal self.MinimumDepth
        with get(): BlockHeightOffset32 = self.ConnectedChannel.MinimumDepth

    member self.ChannelId
        with get(): ChannelIdentifier = self.ConnectedChannel.ChannelId

