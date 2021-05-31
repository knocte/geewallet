namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Diagnostics

open NBitcoin
open DotNetLightning.Chain
open DotNetLightning.Channel
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
        ConnectedChannel: ConnectedChannel
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
        let! channel =
            MonoHopUnidirectionalChannel.Create
                nodeId
                account
                nodeMasterPrivKey
                channelIndex
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
                    ChannelPrivKeys = channel.ChannelPrivKeys
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
                    let fundingParametersRes, channelAfterAcceptChannel =
                        let channelCmd = ApplyAcceptChannel acceptChannelMsg
                        channelAfterOpenChannel.ExecuteCommand channelCmd <| function
                            | (WeAcceptedAcceptChannel(fundingDestination, fundingAmount, _)::[])
                                -> Some (fundingDestination, fundingAmount)
                            | _ -> None
                    match fundingParametersRes with
                    | Error err ->
                        return Error <| InvalidAcceptChannel
                            (peerNodeAfterAcceptChannel, err)
                    | Ok (fundingDestination, fundingAmount) ->
                        assert ((fundingAmount.ToUnit MoneyUnit.BTC) = channelCapacity.ValueToSend)
                        let minimumDepth = acceptChannelMsg.MinimumDepth
                        let connectedChannel = {
                            PeerNode = peerNodeAfterAcceptChannel
                            Channel = channelAfterAcceptChannel
                            Account = account
                            // TODO: move this into FundedChannel?
                            MinimumDepth = minimumDepth
                            ChannelIndex = channelIndex
                        }
                        let outgoingUnfundedChannel = {
                            ConnectedChannel = connectedChannel
                            FundingDestination = fundingDestination
                            TransferAmount = channelCapacity
                        }
                        return Ok outgoingUnfundedChannel
                | _ -> return Error <| ExpectedAcceptChannel channelMsg
    }

    member internal self.MinimumDepth
        with get(): BlockHeightOffset32 = self.ConnectedChannel.MinimumDepth

