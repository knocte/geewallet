namespace GWallet.Backend

open System
open System.Net
open System.Net.Sockets

open GWallet.Backend.FSharpUtil.UwpHacks

type ProtocolGlitchException(message: string, innerException: Exception) =
    inherit CommunicationUnsuccessfulException (message, innerException)

type ServerCannotBeResolvedException =
    inherit CommunicationUnsuccessfulException

    new(message) = { inherit CommunicationUnsuccessfulException(message) }
    new(message:string, innerException: Exception) = { inherit CommunicationUnsuccessfulException(message, innerException) }

type ServerNameResolvedToInvalidAddressException(message: string) =
    inherit CommunicationUnsuccessfulException (message)


type JsonRpcTcpClient (host: string, port: uint32) =

    let ResolveAsync (hostName: string): Async<Option<IPAddress>> = async {
        // FIXME: loop over all addresses?
        let! hostEntry = Dns.GetHostEntryAsync hostName |> Async.AwaitTask
        return hostEntry.AddressList |> Array.tryHead
    }

    let exceptionMsg = "JsonRpcSharp faced some problem when trying communication"

    let ResolveHost(): Async<IPAddress> = async {
        try
            let! maybeTimedOutipAddress = ResolveAsync host |> FSharpUtil.WithTimeout Config.DEFAULT_NETWORK_TIMEOUT
            match maybeTimedOutipAddress with
            | Some ipAddressOption ->
                match ipAddressOption with
                | Some ipAddress ->
                    if ipAddress.ToString().StartsWith("127.0.0.") then
                        let msg = SPrintF2 "Server '%s' resolved to localhost IP '%s'" host (ipAddress.ToString())
                        return raise <| ServerNameResolvedToInvalidAddressException (msg)
                    else
                        return ipAddress
                | None   -> return raise <| ServerCannotBeResolvedException
                                                (SPrintF1 "DNS host entry lookup resulted in no records for %s" host)
            | None -> return raise <| TimeoutException (SPrintF2 "Timed out connecting to %s:%i" host port)
        with
        | :? TimeoutException ->
            return raise(ServerCannotBeResolvedException(exceptionMsg))
        | ex ->
            match FSharpUtil.FindException<SocketException> ex with
            | None ->
                return raise <| FSharpUtil.ReRaise ex
            | Some socketException ->
                if socketException.ErrorCode = int SocketError.HostNotFound ||
                   socketException.ErrorCode = int SocketError.NoData ||
                   socketException.ErrorCode = int SocketError.TryAgain then
                    return raise <| ServerCannotBeResolvedException(exceptionMsg, ex)
                return raise <| UnhandledSocketException(socketException.ErrorCode, ex)
    }

    let rpcTcpClientInnerRequest =
            let tcpClient =
                JsonRpcSharp.TcpClient.JsonRpcClient(ResolveHost, int port, Config.DEFAULT_NETWORK_CONNECT_TIMEOUT)
            fun jsonRequest -> tcpClient.RequestAsync jsonRequest

    member __.Host with get() = host

    member self.Request (request: string): Async<string> = async {
        try
            let! stringOption = rpcTcpClientInnerRequest request |> FSharpUtil.WithTimeout Config.DEFAULT_NETWORK_TIMEOUT
            let str =
                match stringOption with
                | Some s -> s
                | None   -> raise <| ServerTimedOutException("Timeout when trying to communicate with UtxoCoin server")
            return str
        with
        | :? CommunicationUnsuccessfulException as ex ->
            return raise <| FSharpUtil.ReRaise ex
        | :? JsonRpcSharp.TcpClient.CommunicationUnsuccessfulException as ex ->
            return raise <| CommunicationUnsuccessfulException(ex.Message, ex)

        // FIXME: we should log this one on Sentry as a warning because it's really strange, I bet it's a bug
        // on Mono that could maybe go away with higher versions of it (higher versions of Xamarin-Android), see
        // git blame to look at the whole stacktrace (ex.ToString())
        | :? NotSupportedException as nse ->
            return raise <| ProtocolGlitchException(exceptionMsg, nse)
        | ex ->
            match Networking.FindExceptionToRethrow ex exceptionMsg with
            | None ->
                return raise <| FSharpUtil.ReRaise ex
            | Some rewrappedSocketException ->
                return raise rewrappedSocketException
    }
