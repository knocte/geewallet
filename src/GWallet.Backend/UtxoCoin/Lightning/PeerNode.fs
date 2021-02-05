namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Net

open NBitcoin
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Utils
open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks


type internal RecvChannelMsgError =
    | RecvMsg of RecvMsgError
    | ReceivedPeerErrorMessage of PeerNode * PeerErrorMessage
    interface IErrorMsg with
        member this.Message =
            match this with
            | RecvMsg err ->
                SPrintF1 "Error receiving message from peer: %s" (err :> IErrorMsg).Message
            | ReceivedPeerErrorMessage (_, err) ->
                SPrintF1 "Error message from peer: %s" (err :> IErrorMsg).Message

and internal PeerNode =
    {
        InitMsg: InitMsg
        MsgStream: MsgStream
    }
    interface IDisposable with
        member self.Dispose() =
            (self.MsgStream :> IDisposable).Dispose()

    static member internal ConnectFromTransportListener (transportListener: TransportListener)
                                                        (peerNodeId: NodeId)
                                                        (peerId: PeerId)
                                                            : Async<Result<PeerNode, ConnectError>> = async {
        let! connectRes = MsgStream.ConnectFromTransportListener transportListener peerNodeId peerId
        match connectRes with
        | Error connectError -> return Error connectError
        | Ok (initMsg, msgStream) ->
            return Ok {
                InitMsg = initMsg
                MsgStream = msgStream
            }
    }

    static member internal AcceptFromTransportListener (transportListener: TransportListener)
                                                       (peerNodeId: NodeId)
                                                           : Async<Result<PeerNode, ConnectError>> = async {
        let! acceptRes = MsgStream.AcceptFromTransportListener transportListener
        match acceptRes with
        | Error connectError -> return Error connectError
        | Ok (initMsg, msgStream) ->
            if msgStream.RemoteNodeId = peerNodeId then
                return Ok {
                    InitMsg = initMsg
                    MsgStream = msgStream
                }
            else
                (msgStream :> IDisposable).Dispose()
                return! PeerNode.AcceptFromTransportListener transportListener peerNodeId
    }

    static member internal ConnectAcceptFromTransportListener (transportListener: TransportListener)
                                                              (peerNodeId: NodeId)
                                                              (peerId: PeerId)
                                                                  : Async<Result<PeerNode, ConnectError>> = async {
        let! connectRes = MsgStream.ConnectAcceptFromTransportListener transportListener peerNodeId peerId
        match connectRes with
        | Error connectError -> return Error connectError
        | Ok (initMsg, msgStream) ->
            return Ok {
                InitMsg = initMsg
                MsgStream = msgStream
            }
    }

    static member internal AcceptAnyFromTransportListener (transportListener: TransportListener)
                                                              : Async<Result<PeerNode, ConnectError>> = async {
        let! acceptRes = MsgStream.AcceptFromTransportListener transportListener
        match acceptRes with
        | Error connectError -> return Error connectError
        | Ok (initMsg, msgStream) ->
            return Ok {
                InitMsg = initMsg
                MsgStream = msgStream
            }
    }

    member internal self.RemoteNodeId: NodeId =
        self.MsgStream.RemoteNodeId

    member internal self.PeerId: PeerId =
        self.MsgStream.PeerId

    member internal self.RemoteEndPoint: IPEndPoint =
        self.MsgStream.RemoteEndPoint

    member internal self.NodeEndPoint: NodeEndPoint =
        self.MsgStream.NodeEndPoint

    member internal self.NodeSecret: ExtKey =
        self.MsgStream.NodeSecret

    member internal self.SendMsg (msg: ILightningMsg): Async<PeerNode> = async {
        let! msgStream = self.MsgStream.SendMsg msg
        return { self with MsgStream = msgStream }
    }

    member internal self.RecvChannelMsg(): Async<Result<PeerNode * IChannelMsg, RecvChannelMsgError>> =
        let rec recv (msgStream: MsgStream) = async {
            let! recvMsgRes = msgStream.RecvMsg()
            match recvMsgRes with
            | Error recvMsgError -> return Error <| RecvMsg recvMsgError
            | Ok (msgStreamAfterMsgReceived, msg) ->
                match msg with
                | :? ErrorMsg as errorMsg ->
                    let peerNode = { self with MsgStream = msgStreamAfterMsgReceived }
                    return Error <| ReceivedPeerErrorMessage (peerNode, { ErrorMsg = errorMsg })
                | :? PingMsg as pingMsg ->
                    let! msgStreamAfterPongSent = msgStreamAfterMsgReceived.SendMsg { PongMsg.BytesLen = pingMsg.PongLen }
                    return! recv msgStreamAfterPongSent
                | :? PongMsg ->
                    return failwith "sending pings is not implemented"
                | :? InitMsg ->
                    return failwith "unexpected init msg"
                | :? IRoutingMsg ->
                    Infrastructure.LogDebug "handling routing messages is not implemented"
                    return! recv msgStreamAfterMsgReceived
                | :? IChannelMsg as msg ->
                    let peerNode = { self with MsgStream = msgStreamAfterMsgReceived }
                    return Ok (peerNode, msg)
                | _ ->
                    return failwith <| SPrintF1 "unreachable %A" msg
        }
        recv self.MsgStream


