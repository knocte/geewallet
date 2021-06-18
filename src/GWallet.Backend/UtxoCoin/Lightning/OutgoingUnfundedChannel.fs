namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Diagnostics

open NBitcoin
open DotNetLightning.Chain
open DotNetLightning.Channel
open DotNetLightning.Crypto
open DotNetLightning.Serialization.Msgs
open DotNetLightning.Utils
open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.FSharpUtil
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
        member __.ChannelBreakdown: bool =
            false

    member internal self.PossibleBug =
        match self with
        | RecvAcceptChannel err -> err.PossibleBug
        | InvalidChannelParameters _
        | OpenChannelPeerErrorResponse _
        | ExpectedAcceptChannel _
        | InvalidAcceptChannel _ -> false

type internal OutgoingUnfundedChannel =
    {
        PeerNode: PeerNode
        ChannelWaitingForFundingTx: ChannelWaitingForFundingTx
        Account: NormalUtxoAccount
        MinimumDepth: BlockHeightOffset32
        ChannelIndex: int
        FundingDestination: IDestination
        TransferAmount: TransferAmount
    }
    static member OpenChannel (peerNode: PeerNode)
                              (account: NormalUtxoAccount)
                              (channelCapacity: TransferAmount)
                                  : Async<Result<OutgoingUnfundedChannel, OpenChannelError>> = async {
        let nodeId = peerNode.RemoteNodeId
        let nodeMasterPrivKey = peerNode.NodeMasterPrivKey()
        let channelIndex =
            let random = Org.BouncyCastle.Security.SecureRandom() :> Random
            random.Next(1, Int32.MaxValue / 2)
        let currency = (account :> IAccount).Currency
        let! channelOptions = MonoHopUnidirectionalChannel.DefaultChannelOptions (currency)
        let! feeEstimator = FeeEstimator.Create currency
        let network = UtxoCoin.Account.GetNetwork currency
        let defaultFinalScriptPubKey = ScriptManager.CreatePayoutScript account
        let localParams =
            let funding = Money(channelCapacity.ValueToSend, MoneyUnit.BTC)
            Settings.GetLocalParams funding
        let temporaryChannelId = ChannelIdentifier.NewRandom()
        let feeRate =
            (feeEstimator :> IFeeEstimator).GetEstSatPer1000Weight ConfirmationTarget.Normal
        let channelWaitingForAcceptChannelRes =
            Channel.NewOutbound(
                Settings.PeerLimits,
                channelOptions,
                false,
                nodeMasterPrivKey,
                channelIndex,
                network,
                nodeId,
                Some defaultFinalScriptPubKey,
                temporaryChannelId.DnlChannelId,
                Money (channelCapacity.ValueToSend, MoneyUnit.BTC),
                LNMoney 0L,
                feeRate,
                localParams,
                peerNode.InitMsg
            )
        match channelWaitingForAcceptChannelRes with
        | Error channelError ->
            return Error <| InvalidChannelParameters (peerNode, channelError)
        | Ok (openChannelMsg, channelWaitingForAcceptChannel) ->
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
                    let channelWaitingForFundingTxRes =
                        channelWaitingForAcceptChannel.ApplyAcceptChannel
                            acceptChannelMsg
                    match channelWaitingForFundingTxRes with
                    | Error err ->
                        return Error <| InvalidAcceptChannel
                            (peerNodeAfterAcceptChannel, err)
                    | Ok (fundingDestination, fundingAmount, channelWaitingForFundingTx) ->
                        assert ((fundingAmount.ToUnit MoneyUnit.BTC) = channelCapacity.ValueToSend)
                        let minimumDepth = acceptChannelMsg.MinimumDepth
                        let outgoingUnfundedChannel = {
                            PeerNode = peerNodeAfterAcceptChannel
                            ChannelWaitingForFundingTx = channelWaitingForFundingTx
                            Account = account
                            // TODO: move this into FundedChannel?
                            MinimumDepth = minimumDepth
                            ChannelIndex = channelIndex
                            FundingDestination = fundingDestination
                            TransferAmount = channelCapacity
                        }
                        return Ok outgoingUnfundedChannel
                | _ -> return Error <| ExpectedAcceptChannel channelMsg
    }

