namespace GWallet.Backend

open System
open System.Net
open System.Net.Sockets

open GWallet.Backend.FSharpUtil.UwpHacks

// https://en.wikipedia.org/wiki/List_of_HTTP_status_codes#Cloudflare
type CloudFlareError =
    | ConnectionTimeOut = 522
    | WebServerDown = 521
    | OriginUnreachable = 523
    | OriginSslHandshakeError = 525

type internal UnhandledSocketException =
    inherit Exception

    new(socketErrorCode: int, innerException: Exception) =
        { inherit Exception(SPrintF1 "Backend not prepared for this SocketException with ErrorCode[%i]" socketErrorCode,
                                    innerException) }

type CommunicationUnsuccessfulException =
    inherit Exception

    new(message: string, innerException: Exception) = { inherit Exception(message, innerException) }
    new(message: string) = { inherit Exception(message) }
    new() = { inherit Exception() }

type ServerDiscardedException(message: string, innerException: CommunicationUnsuccessfulException) =
   inherit Exception (message, innerException)

type BuggyExceptionFromOldMonoVersion (message: string, innerException: Exception) =
    inherit CommunicationUnsuccessfulException (message, innerException)

type ServerClosedConnectionEarlyException(message: string, innerException: Exception) =
    inherit CommunicationUnsuccessfulException (message, innerException)

type ServerRefusedException(message:string, innerException: Exception) =
    inherit CommunicationUnsuccessfulException (message, innerException)

type ServerTimedOutException =
    inherit CommunicationUnsuccessfulException

    new(message: string, innerException: Exception) =
        { inherit CommunicationUnsuccessfulException(message, innerException) }
    new(message) =
        { inherit CommunicationUnsuccessfulException(message) }

type ServerUnreachableException =
    inherit CommunicationUnsuccessfulException

    new(message: string, innerException: Exception) =
        {
            inherit CommunicationUnsuccessfulException(message, innerException)
        }
    new(message: string, httpStatusCode: HttpStatusCode, innerException: Exception) =
        {
            inherit CommunicationUnsuccessfulException(SPrintF2 "%s (HttpErr: %s)" message (httpStatusCode.ToString()),
                                                    innerException)
        }
    new(message: string, cloudFlareError: CloudFlareError, innerException: Exception) =
        {
            inherit CommunicationUnsuccessfulException(SPrintF2 "%s (CfErr: %s)" message (cloudFlareError.ToString()),
                                                    innerException)
        }

type ServerMisconfiguredException =
    inherit CommunicationUnsuccessfulException

    new (message: string, innerException: Exception) =
        { inherit CommunicationUnsuccessfulException (message, innerException) }
    new (message: string) =
        { inherit CommunicationUnsuccessfulException (message) }

module Networking =

    let FindExceptionToRethrow (ex: Exception) (newExceptionMsg): Option<Exception> =
        match FSharpUtil.FindException<SocketException> ex with
        | None ->
            None
        | Some socketException ->
            if socketException.ErrorCode = int SocketError.ConnectionRefused then
                ServerRefusedException(newExceptionMsg, ex) :> Exception |> Some
            elif socketException.ErrorCode = int SocketError.ConnectionReset then
                ServerRefusedException(newExceptionMsg, ex) :> Exception |> Some

            elif socketException.ErrorCode = int SocketError.TimedOut then
                ServerTimedOutException(newExceptionMsg, ex) :> Exception |> Some

            // probably misleading errorCode (see fixed mono bug: https://github.com/mono/mono/pull/8041 )
            // TODO: remove this when Mono X.Y (where X.Y=version to introduce this bugfix) is stable
            //       everywhere (probably 8 years from now?), and see if we catch it again in sentry
            elif socketException.ErrorCode = int SocketError.AddressFamilyNotSupported then
                ServerUnreachableException(newExceptionMsg, ex) :> Exception |> Some

            // -1!?! WTF, mono bug in v6.4.0? see https://sentry.io/organizations/nblockchain/issues/1261821968/
            elif socketException.ErrorCode = int SocketError.SocketError &&
                 socketException.Message.Contains "mono-io-layer-error" then
                ServerUnreachableException(newExceptionMsg, ex) :> Exception |> Some

            elif socketException.ErrorCode = int SocketError.HostUnreachable then
                ServerUnreachableException(newExceptionMsg, ex) :> Exception |> Some
            elif socketException.ErrorCode = int SocketError.NetworkUnreachable then
                ServerUnreachableException(newExceptionMsg, ex) :> Exception |> Some
            elif socketException.ErrorCode = int SocketError.AddressNotAvailable then
                ServerUnreachableException(newExceptionMsg, ex) :> Exception |> Some
            elif socketException.ErrorCode = int SocketError.NetworkDown then
                ServerUnreachableException(newExceptionMsg, ex) :> Exception |> Some
            elif socketException.ErrorCode = int SocketError.Shutdown then
                ServerClosedConnectionEarlyException(newExceptionMsg, ex) :> Exception |> Some
            elif socketException.ErrorCode = int SocketError.ProtocolOption then
                ServerUnreachableException(newExceptionMsg, ex) :> Exception |> Some
            elif socketException.ErrorCode = int SocketError.HostNotFound then
                ServerUnreachableException(newExceptionMsg, ex) :> Exception |> Some

            else
                UnhandledSocketException(socketException.ErrorCode, ex) :> Exception |> Some

