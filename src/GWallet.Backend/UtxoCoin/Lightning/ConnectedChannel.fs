namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Net

open NBitcoin
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Channel
open DotNetLightning.Utils
open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks
open GWallet.Backend.UtxoCoin


type internal ReestablishError =
    | RecvReestablish of RecvMsgError
    | PeerErrorResponse of PeerNode * PeerErrorMessage
    | ExpectedReestablishMsg of ILightningMsg
    | ExpectedReestablishOrFundingLockedMsg of ILightningMsg
    interface IErrorMsg with
        member self.Message =
            match self with
            | RecvReestablish err ->
                SPrintF1 "Error receiving channel_reestablish: %s" (err :> IErrorMsg).Message
            | PeerErrorResponse (_, err) ->
                SPrintF1 "Peer responded to our channel_reestablish with an error: %s" (err :> IErrorMsg).Message
            | ExpectedReestablishMsg msg ->
                SPrintF1 "Expected channel_reestablish, got %A" (msg.GetType())
            | ExpectedReestablishOrFundingLockedMsg msg ->
                SPrintF1 "Expected channel_reestablish or funding_locked, got %A" (msg.GetType())
    member internal self.PossibleBug =
        match self with
        | RecvReestablish err -> err.PossibleBug
        | PeerErrorResponse _
        | ExpectedReestablishMsg _
        | ExpectedReestablishOrFundingLockedMsg _ -> false

type internal ReconnectError =
    | Connect of ConnectError
    | Reestablish of ReestablishError
    interface IErrorMsg with
        member self.Message =
            match self with
            | Connect err ->
                SPrintF1 "Error reconnecting to peer: %s" (err :> IErrorMsg).Message
            | Reestablish err ->
                SPrintF1 "Error reestablishing channel with connected peer: %s" (err :> IErrorMsg).Message
    member internal self.PossibleBug =
        match self with
        | Connect err -> err.PossibleBug
        | Reestablish err -> err.PossibleBug

type internal ConnectedChannel =
    {
        PeerNode: PeerNode
        Channel: MonoHopUnidirectionalChannel
        Account: NormalUtxoAccount
        MinimumDepth: BlockHeightOffset32
        ChannelIndex: int
    }
    interface IDisposable with
        member self.Dispose() =
            (self.PeerNode :> IDisposable).Dispose()

    static member private LoadChannel (channelStore: ChannelStore)
                                      (nodeSecretKey: ExtKey)
                                      (channelId: ChannelIdentifier)
                                          : Async<SerializedChannel * MonoHopUnidirectionalChannel> = async {
        let serializedChannel = channelStore.LoadChannel channelId
        Infrastructure.LogDebug <| SPrintF1 "loading channel for %s" (channelId.ToString())
        let! channel =
            let fundingTxProvider (_ : IDestination * Money * FeeRatePerKw) =
                Result.Error "funding tx not needed cause channel already created"
            MonoHopUnidirectionalChannel.Create
                serializedChannel.RemoteNodeId
                channelStore.Account
                nodeSecretKey
                serializedChannel.ChannelIndex
                fundingTxProvider
                serializedChannel.ChanState
        return serializedChannel, channel
    }

    static member private Reestablish (peerNode: PeerNode)
                                      (channel: MonoHopUnidirectionalChannel)
                                          : Async<Result<PeerNode * MonoHopUnidirectionalChannel, ReestablishError>> = async {
        let channelId =
            match channel.ChannelId with
            | Some channelId -> channelId
            | None ->
                failwith
                    "A channel can only be reestablished if it has previously been \
                    established and therefore has a channel id"

        let ourReestablishMsgRes, channelAfterReestablishSent =
            let channelCmd = ChannelCommand.CreateChannelReestablish
            channel.ExecuteCommand channelCmd <| function
                | WeSentChannelReestablish ourReestablishMsg::[] ->
                    Some ourReestablishMsg
                | _ -> None
        let ourReestablishMsg = UnwrapResult ourReestablishMsgRes "error executing channel reestablish command"

        Infrastructure.LogDebug <| SPrintF1 "sending reestablish for %s" (channelId.ToString())
        let! peerNodeAfterReestablishSent = peerNode.SendMsg ourReestablishMsg
        Infrastructure.LogDebug <| SPrintF1 "receiving reestablish for %s" (channelId.ToString())
        let! reestablishRes = async {
            let! recvMsgRes = peerNodeAfterReestablishSent.RecvChannelMsg()
            match recvMsgRes with
            | Error (RecvMsg recvMsgError) -> return Error <| RecvReestablish recvMsgError
            | Error (ReceivedPeerErrorMessage (peerNodeAfterNextMsgReceived, errorMessage)) ->
                return Error <| PeerErrorResponse (peerNodeAfterNextMsgReceived, errorMessage)
            | Ok (peerNodeAfterNextMsgReceived, channelMsg) ->
                match channelMsg with
                | :? ChannelReestablishMsg as reestablishMsg ->
                    return Ok (peerNodeAfterNextMsgReceived, reestablishMsg)
                | :? FundingLockedMsg ->
                    let! recvMsgRes = peerNodeAfterNextMsgReceived.RecvChannelMsg()
                    match recvMsgRes with
                    | Error (RecvMsg recvMsgError) -> return Error <| RecvReestablish recvMsgError
                    | Error (ReceivedPeerErrorMessage (peerNodeAfterReestablishReceived, errorMessage)) ->
                        return Error <| PeerErrorResponse
                            (peerNodeAfterReestablishReceived, errorMessage)
                    | Ok (peerNodeAfterReestablishReceived, channelMsg) ->
                        match channelMsg with
                        | :? ChannelReestablishMsg as reestablishMsg ->
                            return Ok (peerNodeAfterReestablishReceived, reestablishMsg)
                        | msg ->
                            return Error <| ExpectedReestablishMsg msg
                | msg ->
                    return Error <| ExpectedReestablishOrFundingLockedMsg msg
        }
        match reestablishRes with
        | Error err -> return Error err
        | Ok (peerNodeAfterReestablishReceived, _theirReestablishMsg) ->
            // TODO: check their reestablish msg
            //
            // A channel_reestablish message contains the channel ID as well as
            // information specifying what state the remote node thinks the channel
            // is in. So we need to check that the channel IDs match, validate that
            // the information they've sent us makes sense, and possibly re-send
            // commitments. Aside from checking the channel ID this is the sort of
            // thing that should be handled by DNL, except DNL doesn't have an
            // ApplyChannelReestablish command.
            return Ok (peerNodeAfterReestablishReceived, channelAfterReestablishSent)
    }

    static member internal ConnectFromWallet (channelStore: ChannelStore)
                                             (nodeSecret: ExtKey)
                                             (channelId: ChannelIdentifier)
                                                 : Async<Result<ConnectedChannel, ReconnectError>> = async {
        let! serializedChannel, channel =
            ConnectedChannel.LoadChannel channelStore nodeSecret channelId
        let! connectRes =
            let nodeId = channel.RemoteNodeId
            let peerId = PeerId (serializedChannel.CounterpartyIP :> EndPoint)
            PeerNode.Connect nodeSecret nodeId peerId
        match connectRes with
        | Error connectError -> return Error <| Connect connectError
        | Ok peerNode ->
            let! reestablishRes =
                ConnectedChannel.Reestablish peerNode channel
            match reestablishRes with
            | Error reestablishError -> return Error <| Reestablish reestablishError
            | Ok (peerNodeAfterReestablish, channelAfterReestablish) ->
                let minimumDepth = serializedChannel.MinSafeDepth
                let channelIndex = serializedChannel.ChannelIndex
                let connectedChannel = {
                    Account = channelStore.Account
                    Channel = channelAfterReestablish
                    PeerNode = peerNodeAfterReestablish
                    MinimumDepth = minimumDepth
                    ChannelIndex = channelIndex
                }
                return Ok connectedChannel
    }

    static member internal AcceptFromWallet (channelStore: ChannelStore)
                                            (transportListener: TransportListener)
                                            (channelId: ChannelIdentifier)
                                                : Async<Result<ConnectedChannel, ReconnectError>> = async {
        let! serializedChannel, channel =
            ConnectedChannel.LoadChannel channelStore transportListener.NodeSecret channelId
        let! connectRes =
            let nodeId = channel.RemoteNodeId
            let peerId = PeerId (serializedChannel.CounterpartyIP :> EndPoint)
            PeerNode.ConnectAcceptFromTransportListener
                transportListener
                nodeId
                peerId
        match connectRes with
        | Error connectError -> return Error <| Connect connectError
        | Ok peerNode ->
            let! reestablishRes =
                ConnectedChannel.Reestablish peerNode channel
            match reestablishRes with
            | Error reestablishError -> return Error <| Reestablish reestablishError
            | Ok (peerNodeAfterReestablish, channelAfterReestablish) ->
                let minimumDepth = serializedChannel.MinSafeDepth
                let channelIndex = serializedChannel.ChannelIndex
                let connectedChannel = {
                    Account = channelStore.Account
                    Channel = channelAfterReestablish
                    PeerNode = peerNodeAfterReestablish
                    MinimumDepth = minimumDepth
                    ChannelIndex = channelIndex
                }
                return Ok connectedChannel
    }

    member self.SaveToWallet() =
        let channelStore = ChannelStore self.Account
        let serializedChannel = {
            ChannelIndex = self.ChannelIndex
            Network = self.Channel.Network
            RemoteNodeId = self.PeerNode.RemoteNodeId
            ChanState = self.Channel.Channel.State
            AccountFileName = self.Account.AccountFile.Name
            CounterpartyIP = self.PeerNode.RemoteEndPoint
            MinSafeDepth = self.MinimumDepth
        }
        channelStore.SaveChannel serializedChannel

    member internal self.RemoteNodeId
        with get(): NodeId = self.Channel.RemoteNodeId

    member internal self.Network
        with get(): Network = self.Channel.Network

    member self.ChannelId
        with get(): ChannelIdentifier =
            UnwrapOption
                self.Channel.ChannelId
                "A ConnectedChannel guarantees that a channel is connected and \
                therefore has a channel id"

    member self.FundingTxId
        with get(): TransactionIdentifier =
            UnwrapOption
                self.Channel.FundingTxId
                "A ConnectedChannel guarantees that a channel has been \
                established and therefore has a funding txid"

    member internal self.FundingScriptCoin
        with get(): Option<ScriptCoin> = self.Channel.FundingScriptCoin

    member self.SendError (err: string): Async<ConnectedChannel> = async {
        let errorMsg = {
            ChannelId =
                match self.Channel.Channel.State.ChannelId with
                | Some channelId -> WhichChannel.SpecificChannel channelId
                | _ -> WhichChannel.All
            Data = System.Text.Encoding.ASCII.GetBytes err
        }
        let! peerNode = self.PeerNode.SendMsg errorMsg
        return {
            self with
                PeerNode = peerNode
        }
    }

