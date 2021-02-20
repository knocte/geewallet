namespace GWallet.Backend.Ether

open System
open System.Net
open System.Runtime.Serialization

open GWallet.Backend
open GWallet.Backend.FSharpUtil.UwpHacks

type HttpStatusCodeNotPresentInTheBcl =
    | TooManyRequests = 429
    | FrozenSite = 530

type RpcErrorCode =
    // "This request is not supported because your node is running with state pruning. Run with --pruning=archive."
    | StatePruningNodeOrMissingTrieNodeOrHeaderNotFound = -32000

    // ambiguous or generic because I've seen same code applied to two different error messages already:
    // "Transaction with the same hash was already imported. (Transaction with the same hash was already imported.)"
    // AND
    // "There are too many transactions in the queue. Your transaction was dropped due to limit.
    //  Try increasing the fee. (There are too many transactions in the queue. Your transaction was dropped due to
    //  limit. Try increasing the fee.)"
    | TransactionAlreadyImportedOrTooManyTransactionsInTheQueue = -32010

    | UnknownBlockNumber = -32602
    | GatewayTimeout = -32050
    | EmptyResponse = -32042


type ServerCannotBeResolvedException =
    inherit CommunicationUnsuccessfulException

    new (message: string, innerException: Exception) =
        { inherit CommunicationUnsuccessfulException (message, innerException) }

    new (info: SerializationInfo, context: StreamingContext) =
        { inherit CommunicationUnsuccessfulException (info, context) }

type ServerUnavailableException =
    inherit CommunicationUnsuccessfulException

    new (message: string, innerException: Exception) =
        { inherit CommunicationUnsuccessfulException (message, innerException) }

    new (info: SerializationInfo, context: StreamingContext) =
        { inherit CommunicationUnsuccessfulException (info, context) }

type ServerChannelNegotiationException =
    inherit CommunicationUnsuccessfulException

    new (message: string, innerException: Exception) =
        { inherit CommunicationUnsuccessfulException (message, innerException) }

    new (message: string,
         webExStatusCode: WebExceptionStatus,
         innerException: Exception) =
        { inherit CommunicationUnsuccessfulException (SPrintF2
                                                          "%s (WebErr: %s)"
                                                          message
                                                          (webExStatusCode.ToString ()),
                                                      innerException) }

    new (message: string,
         cloudFlareError: CloudFlareError,
         innerException: Exception) =
        { inherit CommunicationUnsuccessfulException (SPrintF2
                                                          "%s (CfErr: %s)"
                                                          message
                                                          (cloudFlareError.ToString ()),
                                                      innerException) }

    new (info: SerializationInfo, context: StreamingContext) =
        { inherit CommunicationUnsuccessfulException (info, context) }

type ServerRestrictiveException =
    inherit CommunicationUnsuccessfulException

    new (message: string, innerException: Exception) =
        { inherit CommunicationUnsuccessfulException (message, innerException) }

    new (info: SerializationInfo, context: StreamingContext) =
        { inherit CommunicationUnsuccessfulException (info, context) }

type UnhandledWebException =
    inherit Exception

    new (status: WebExceptionStatus, innerException: Exception) =
        { inherit Exception (SPrintF1
                                 "Backend not prepared for this WebException with Status[%i]"
                                 (int status),
                             innerException) }

    new (info: SerializationInfo, context: StreamingContext) =
        { inherit Exception (info, context) }
