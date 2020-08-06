namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Net.Sockets
open System.Net
open System.Diagnostics

open NBitcoin
open DotNetLightning.Peer
open DotNetLightning.Utils
open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks

type IErrorMsg =
    abstract member Message: string

type PeerDisconnectedError =
    {
        Abruptly: bool
    }
    interface IErrorMsg with
        member self.Message =
            if self.Abruptly then
                "peer disconnected after sending a partial message"
            else
                "peer disconnected"
    member internal self.PossibleBug =
        not self.Abruptly

type HandshakeError =
    | TcpConnect of seq<SocketException>
    | TcpAccept of seq<SocketException>
    | DisconnectedOnAct1 of PeerDisconnectedError
    | InvalidAct1 of PeerError
    | DisconnectedOnAct2 of PeerDisconnectedError
    | InvalidAct2 of PeerError
    | DisconnectedOnAct3 of PeerDisconnectedError
    | InvalidAct3 of PeerError
    interface IErrorMsg with
        member self.Message =
            match self with
            | TcpConnect errs ->
                let messages = Seq.map (fun (err: SocketException) -> err.Message) errs
                SPrintF1 "TCP connection failed: %s" (String.concat "; " messages)
            | TcpAccept errs ->
                let messages = Seq.map (fun (err: SocketException) -> err.Message) errs
                SPrintF1 "TCP accept failed: %s" (String.concat "; " messages)
            | DisconnectedOnAct1 err ->
                SPrintF1 "Peer disconnected before starting handshake: %s" (err :> IErrorMsg).Message
            | InvalidAct1 err ->
                SPrintF1 "Invalid handshake act 1: %s" err.Message
            | DisconnectedOnAct2 err ->
                SPrintF1 "Peer disconnected before sending handshake act 2: %s" (err :> IErrorMsg).Message
            | InvalidAct2 err ->
                SPrintF1 "Invalid handshake act 2: %s" err.Message
            | DisconnectedOnAct3 err ->
                SPrintF1 "Peer disconnected before sending handshake act 3: %s" (err :> IErrorMsg).Message
            | InvalidAct3 err ->
                SPrintF1 "Invalid handshake act 3: %s" err.Message
    member internal self.PossibleBug =
        match self with
        | DisconnectedOnAct1 _
        | DisconnectedOnAct2 _
        | DisconnectedOnAct3 _ -> true
        | TcpConnect _
        | TcpAccept _
        | InvalidAct1 _
        | InvalidAct2 _
        | InvalidAct3 _ -> false

type RecvBytesError =
    | PeerDisconnected of PeerDisconnectedError
    | Decryption of PeerError
    interface IErrorMsg with
        member self.Message =
            match self with
            | PeerDisconnected err ->
                SPrintF1 "Peer disconnected: %s" (err :> IErrorMsg).Message
            | Decryption err ->
                SPrintF1 "Error decrypting message from peer: %s" err.Message
    member internal self.PossibleBug =
        match self with
        | PeerDisconnected err -> err.PossibleBug
        | Decryption _ -> false

type internal TransportListener =
    internal {
        NodeSecret: ExtKey
        Listener: TcpListener
    }
    interface IDisposable with
        member self.Dispose() =
            self.Listener.Stop()

    static member internal Bind (nodeSecret: ExtKey) (endpoint: IPEndPoint) =
        let listener = new TcpListener (endpoint)
        listener.ExclusiveAddressUse <- false
        listener.Server.SetSocketOption(
            SocketOptionLevel.Socket,
            SocketOptionName.ReuseAddress,
            true
        )
        listener.Start()

        {
            NodeSecret = nodeSecret
            Listener = listener
        }

    member internal self.PubKey: PubKey =
        self.NodeSecret.PrivateKey.PubKey

    member internal self.LocalIPEndPoint: IPEndPoint =
        // sillly .NET API: downcast is even done in the sample from the docs: https://docs.microsoft.com/en-us/dotnet/api/system.net.sockets.tcplistener.localendpoint?view=netcore-3.1
        self.Listener.LocalEndpoint :?> IPEndPoint

    member internal self.NodeId: NodeId =
        self.PubKey |> NodeId

    member internal self.EndPoint: NodeEndPoint =
        let nodeId = PublicKey self.PubKey
        NodeEndPoint.FromParts nodeId self.LocalIPEndPoint


type PeerErrorMessage =
    {
        ErrorMsg: DotNetLightning.Serialize.Msgs.ErrorMsg
    }
    interface IErrorMsg with
        member self.Message =
            if self.ErrorMsg.Data.Length = 1 then
                let code = self.ErrorMsg.Data.[0]
                (SPrintF1 "Error code %i received from lightning peer: " code) +
                match code with
                | 0x01uy ->
                    "The number of pending channels exceeds the policy limit.\n\
                    Hint: You can try from a new node identity."
                | 0x02uy ->
                    "Node is not synced to blockchain." +
                    if Config.BitcoinNet() = Network.RegTest then
                        "\nHint: Try mining some blocks before opening."
                    else
                        String.Empty
                | 0x03uy ->
                    "Channel capacity too large.\n\
                    Hint: Try with a smaller funding amount."
                | _ ->
                    "(unknown error code)"
            else
                System.Text.ASCIIEncoding.ASCII.GetString self.ErrorMsg.Data

type internal TransportStream =
    internal {
        NodeSecret: ExtKey
        Peer: Peer
        Client: TcpClient
    }
    interface IDisposable with
        member self.Dispose() =
            self.Client.Close()

    static member private bolt08EncryptedMessageLengthPrefixLength = 18
    static member private bolt08EncryptedMessageMacLength = 16
    // https://github.com/lightningnetwork/lightning-rfc/blob/master/08-transport.md#authenticated-key-exchange-handshake-specification
    static member private bolt08ActOneLength = 50
    static member private bolt08ActTwoLength = 50
    static member private bolt08ActThreeLength = 66

    static member private ReadExactAsync (stream: NetworkStream)
                                         (numberBytesToRead: int)
                                             : Async<Result<array<byte>, PeerDisconnectedError>> =
        let buf: array<byte> = Array.zeroCreate numberBytesToRead
        let rec read buf totalBytesRead = async {
            let! bytesRead =
                stream.ReadAsync(buf, totalBytesRead, (numberBytesToRead - totalBytesRead))
                |> Async.AwaitTask
            let totalBytesRead = totalBytesRead + bytesRead
            if bytesRead = 0 then
                if totalBytesRead = 0 then
                    return Error { Abruptly = false }
                else
                    return Error { Abruptly = true }
            else
                if totalBytesRead < numberBytesToRead then
                    return! read buf totalBytesRead
                else
                    return Ok buf
        }
        read buf 0

    static member private TcpConnect (localEndPoint: IPEndPoint)
                                     (remoteEndPoint: IPEndPoint)
                                         : Async<Result<TcpClient, seq<SocketException>>> = async {
        let client = new TcpClient (remoteEndPoint.AddressFamily)
        client.Client.ExclusiveAddressUse <- false
        client.Client.SetSocketOption(
            SocketOptionLevel.Socket,
            SocketOptionName.ReuseAddress,
            true
        )
        client.Client.Bind(localEndPoint)
        Infrastructure.LogDebug <| SPrintF1 "Connecting over TCP to %A..." remoteEndPoint
        try
            do! client.ConnectAsync(remoteEndPoint.Address, remoteEndPoint.Port) |> Async.AwaitTask
            return Ok client
        with
        | ex ->
            client.Close()
            let socketExceptions = FindSingleException<SocketException> ex
            return Error socketExceptions
        }

    static member private TcpAcceptAny (listener: TcpListener)
                                               : Async<Result<TcpClient, seq<SocketException>>> = async {
        try
            let! client = listener.AcceptTcpClientAsync() |> Async.AwaitTask
            return Ok client
        with
        | ex ->
            let socketExceptions = FindSingleException<SocketException> ex
            return Error socketExceptions
    }

    static member private ConnectHandshake (client: TcpClient)
                                           (nodeSecret: ExtKey)
                                           (peerNodeId: NodeId)
                                               : Async<Result<TransportStream, HandshakeError>> = async {
        let nodeSecretKey = nodeSecret.PrivateKey
        let stream = client.GetStream()
        let peerId = PeerId client.Client.RemoteEndPoint
        let peer = Peer.CreateOutbound(peerId, peerNodeId, nodeSecretKey)
        let act1, peerEncryptor = PeerChannelEncryptor.getActOne peer.ChannelEncryptor
        Debug.Assert((TransportStream.bolt08ActOneLength = act1.Length), "act1 has wrong length")
        let peerAfterAct1 = { peer with ChannelEncryptor = peerEncryptor }

        // Send act1
        do! stream.WriteAsync(act1, 0, act1.Length) |> Async.AwaitTask

        // Receive act2
        Infrastructure.LogDebug "Receiving Act 2..."
        let! act2Res = TransportStream.ReadExactAsync stream TransportStream.bolt08ActTwoLength
        match act2Res with
        | Error peerDisconnectedError -> return Error <| DisconnectedOnAct2 peerDisconnectedError
        | Ok act2 ->
            let peerCmd = ProcessActTwo(act2, nodeSecretKey)
            match Peer.executeCommand peerAfterAct1 peerCmd with
            | Error err -> return Error <| InvalidAct2 err
            | Ok (ActTwoProcessed ((act3, _), _) as evt::[]) ->
                let peerAfterAct2 = Peer.applyEvent peerAfterAct1 evt

                Debug.Assert((TransportStream.bolt08ActThreeLength = act3.Length), SPrintF1 "act3 has wrong length (not %i)" TransportStream.bolt08ActThreeLength)

                do! stream.WriteAsync(act3, 0, act3.Length) |> Async.AwaitTask
                return Ok {
                    NodeSecret = nodeSecret
                    Peer = peerAfterAct2
                    Client = client
                }
            | Ok evts ->
                return failwith <| SPrintF1
                    "DNL returned unexpected events when processing act2: %A" evts
    }

    static member internal ConnectFromTransportListener (transportListener: TransportListener)
                                                        (peerNodeId: NodeId)
                                                        (peerId: PeerId)
                                                            : Async<Result<TransportStream, HandshakeError>> = async {
        let peerEndpoint = peerId.Value :?> IPEndPoint
        let! connectRes = TransportStream.TcpConnect transportListener.LocalIPEndPoint peerEndpoint

        match connectRes with
        | Error err -> return Error <| TcpConnect err
        | Ok client ->
            return! TransportStream.ConnectHandshake client transportListener.NodeSecret peerNodeId
    }

    static member internal AcceptFromTransportListener (transportListener: TransportListener)
                                                           : Async<Result<TransportStream, HandshakeError>> = async {
        let! clientRes = TransportStream.TcpAcceptAny transportListener.Listener
        match clientRes with
        | Error socketError -> return Error <| TcpAccept socketError
        | Ok client ->
            let nodeSecretKey = transportListener.NodeSecret.PrivateKey
            let stream = client.GetStream()
            let peerId = client.Client.RemoteEndPoint |> PeerId
            let peer = Peer.CreateInbound(peerId, nodeSecretKey)
            let! act1Res = TransportStream.ReadExactAsync stream TransportStream.bolt08ActOneLength
            match act1Res with
            | Error peerDisconnectedError -> return Error <| DisconnectedOnAct1 peerDisconnectedError
            | Ok act1 ->
                let peerCmd = ProcessActOne(act1, nodeSecretKey)
                match Peer.executeCommand peer peerCmd with
                | Error err -> return Error <| InvalidAct1 err
                | Ok (ActOneProcessed(act2, _) as evt::[]) ->
                    let peerAfterAct2 = Peer.applyEvent peer evt
                    do! stream.WriteAsync(act2, 0, act2.Length) |> Async.AwaitTask
                    let! act3Res = TransportStream.ReadExactAsync stream TransportStream.bolt08ActThreeLength
                    match act3Res with
                    | Error peerDisconnectedError ->
                        return Error <| DisconnectedOnAct3 peerDisconnectedError
                    | Ok act3 ->
                        let peerCmd = ProcessActThree act3
                        match Peer.executeCommand peerAfterAct2 peerCmd with
                        | Error err -> return Error <| InvalidAct3 err
                        | Ok (ActThreeProcessed(_, _) as evt::[]) ->
                            let peerAfterAct3 = Peer.applyEvent peerAfterAct2 evt

                            return Ok {
                                NodeSecret = transportListener.NodeSecret
                                Peer = peerAfterAct3
                                Client = client
                            }
                        | Ok evts ->
                            return failwith <| SPrintF1
                                "DNL returned unexpected events when processing act3: %A" evts
                | Ok evts ->
                    return failwith <| SPrintF1
                        "DNL returned unexpected events when processing act1: %A" evts
    }

    static member internal ConnectAcceptFromTransportListener (transportListener: TransportListener)
                                                              (peerNodeId: NodeId)
                                                              (peerId: PeerId)
                                                                  : Async<Result<TransportStream, HandshakeError>> = async {
        let localEndPoint = transportListener.LocalIPEndPoint
        let remoteEndPoint = peerId.Value :?> IPEndPoint
        let rec connect (backoff: TimeSpan) = async {
            let! connectRes = TransportStream.TcpConnect localEndPoint remoteEndPoint
            match connectRes with
            | Error errors ->
                let message =
                    let messages = Seq.map (fun (err: SocketException) -> err.Message) errors
                    String.concat "; " messages
                let backoffMillis = (int backoff.TotalMilliseconds)
                Infrastructure.LogDebug <| SPrintF1 "connect errors: %s" message
                Infrastructure.LogDebug <| SPrintF1 "retrying in %ims" backoffMillis
                do! Async.Sleep backoffMillis
                return! connect (backoff + backoff)
            | Ok client -> return Choice1Of2 client
        }

        let initialInterval = TimeSpan.FromSeconds 1.0

        let rec accept (backoff: TimeSpan) = async {
            let! acceptRes = TransportStream.AcceptFromTransportListener transportListener
            match acceptRes with
            | Error error ->
                let backoffMillis = (int backoff.TotalMilliseconds)
                Infrastructure.LogDebug <| SPrintF1 "accept error: %s" (error :> IErrorMsg).Message
                Infrastructure.LogDebug <| SPrintF1 "retrying in %ims" backoffMillis
                do! Async.Sleep backoffMillis
                return! accept (backoff + backoff)
            | Ok transportStream ->
                if transportStream.RemoteNodeId = peerNodeId then
                    return Choice2Of2 transportStream
                else
                    (transportStream :> IDisposable).Dispose()
                    return! accept initialInterval
        }

        let! res = AsyncExtensions.WhenAny [ connect initialInterval; accept initialInterval ]
        match res with
        | Choice1Of2 client ->
            return! TransportStream.ConnectHandshake client transportListener.NodeSecret peerNodeId
        | Choice2Of2 transportStream ->
            return Ok transportStream
    }

    member internal self.RemoteNodeId
        with get(): NodeId =
            match self.Peer.TheirNodeId with
            | Some nodeId ->
                nodeId
            | None -> 
                failwith
                    "The TransportStream type is created by performing a handshake \
                    and in doing so guarantees that the peer's node id is known"

    member internal self.PeerId
        with get(): PeerId = self.Peer.PeerId

    member internal self.RemoteEndPoint
        with get(): IPEndPoint = self.Client.Client.RemoteEndPoint :?> IPEndPoint

    member internal self.NodeEndPoint: NodeEndPoint =
        let remoteNodeId = PublicKey self.RemoteNodeId.Value
        NodeEndPoint.FromParts remoteNodeId self.RemoteEndPoint

    member internal self.SendBytes (plaintext: array<byte>): Async<TransportStream> = async {
        let peer = self.Peer
        let ciphertext, channelEncryptor =
            PeerChannelEncryptor.encryptMessage plaintext peer.ChannelEncryptor
        let peerAfterBytesSent = { peer with ChannelEncryptor = channelEncryptor }
        let stream = self.Client.GetStream()
        do! stream.WriteAsync(ciphertext, 0, ciphertext.Length) |> Async.AwaitTask
        return { self with Peer = peerAfterBytesSent }
    }

    member internal self.RecvBytes(): Async<Result<TransportStream * array<byte>, RecvBytesError>> = async {
        let peer = self.Peer
        let stream = self.Client.GetStream()
        let! encryptedLengthRes =
            TransportStream.ReadExactAsync stream TransportStream.bolt08EncryptedMessageLengthPrefixLength
        match encryptedLengthRes with
        | Error peerDisconnectedError -> return Error <| PeerDisconnected peerDisconnectedError
        | Ok encryptedLength ->
            let decryptLengthRes =
                PeerChannelEncryptor.decryptLengthHeader encryptedLength peer.ChannelEncryptor
            match decryptLengthRes with
            | Error peerError -> return Error <| Decryption peerError
            | Ok (length, channelEncryptor) ->
                let! ciphertextRes =
                    TransportStream.ReadExactAsync
                        stream
                        (int length + TransportStream.bolt08EncryptedMessageMacLength)
                match ciphertextRes with
                | Error _peerDisconnectedError -> return Error <| PeerDisconnected { Abruptly = true }
                | Ok ciphertext ->
                    let decryptBodyRes =
                        PeerChannelEncryptor.decryptMessage ciphertext channelEncryptor
                    match decryptBodyRes with
                    | Error peerError -> return Error <| Decryption peerError
                    | Ok (plaintext, channelEncryptor) ->
                        let peerAfterBytesReceived = { peer with ChannelEncryptor = channelEncryptor }
                        let transportStream = { self with Peer = peerAfterBytesReceived }
                        return Ok (transportStream, plaintext)
    }

