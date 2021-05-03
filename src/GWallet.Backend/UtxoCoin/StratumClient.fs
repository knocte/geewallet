namespace GWallet.Backend.UtxoCoin

open System

open Newtonsoft.Json

open GWallet.Backend
open GWallet.Backend.FSharpUtil.UwpHacks

// can't make this type below private, or else Newtonsoft.Json will serialize it incorrectly
type Request =
    {
        Id: int;
        Method: string;
        Params: obj
    }

type ServerVersionResult =
    {
        Id: int;
        Result: array<string>;
    }

type BlockchainScriptHashGetBalanceInnerResult =
    {
        Confirmed: Int64;
        Unconfirmed: Int64;
    }
type BlockchainScriptHashGetBalanceResult =
    {
        Id: int;
        Result: BlockchainScriptHashGetBalanceInnerResult
    }

type BlockchainScriptHashListUnspentInnerResult =
    {
        TxHash: string;
        TxPos: int;
        Value: Int64;
        Height: Int64;
    }
type BlockchainScriptHashListUnspentResult =
    {
        Id: int;
        Result: array<BlockchainScriptHashListUnspentInnerResult>
    }

type BlockchainScriptHashHistoryInnerResult =
    {
        TxHash: string
        Height: uint32
    }

type BlockchainScriptHashMerkleInnerResult =
    {
        BlockHeight: uint32
        Merkle: List<string>
        Pos: uint32
    }

type BlockchainScriptHashHistoryResult =
    {
        Id: int
        Result: List<BlockchainScriptHashHistoryInnerResult>
    }

type BlockchainScriptHashMerkleResult =
    {
        Id: int
        Result: BlockchainScriptHashMerkleInnerResult
    }

type BlockchainTransactionGetResult =
    {
        Id: int;
        Result: string;
    }

type BlockchainTransactionGetVerboseInnerResult =
    {
        Locktime: uint32
        Confirmations: uint32
        Hex: string
    }

type BlockchainTransactionGetVerboseResult =
    {
        Id: int
        Result: BlockchainTransactionGetVerboseInnerResult
    }

type BlockchainEstimateFeeResult =
    {
        Id: int;
        Result: decimal;
    }

type BlockchainTransactionBroadcastResult =
    {
        Id: int;
        Result: string;
    }

type BlockchainHeadersSubscribeInnerResult =
    {
        Height: int
        Hex: string
    }

type BlockchainHeadersSubscribeResult =
    {
        Id: int
        Result: BlockchainHeadersSubscribeInnerResult
    }

type ErrorInnerResult =
    {
        Message: string;
        Code: int;
    }

type ErrorResult =
    {
        Id: int;
        Error: ErrorInnerResult;
    }

type RpcErrorCode =
    // see https://gitlab.com/nblockchain/geewallet/issues/110
    | ExcessiveResourceUsage = -101

    // see https://gitlab.com/nblockchain/geewallet/issues/117
    | ServerBusy = -102

    // see git commit msg of 0aba03a8291daa526fde888d0c02a789abe411f2
    | InternalError = -32603

    // see https://gitlab.com/nblockchain/geewallet/issues/112
    | UnknownMethod = -32601

type public ElectrumServerReturningImproperJsonResponseException(message: string, innerEx: Exception) =
    inherit ServerMisconfiguredException (message, innerEx)

type public ElectrumServerReturningErrorInJsonResponseException(message: string, code: Option<int>) =
    inherit CommunicationUnsuccessfulException(message)

    member val ErrorCode: Option<int> =
        code with get

    // let's only use this property for the sake of a workaround for this: https://github.com/romanz/electrs/issues/313
    member internal __.ErrorMessage: string = message

type public ElectrumServerReturningErrorException(message: string, code: Option<int>,
                                                  originalRequest: string, originalResponse: string) =
    inherit ElectrumServerReturningErrorInJsonResponseException(message, code)

    member val OriginalRequest: string =
        originalRequest with get

    member val OriginalResponse: string =
        originalResponse with get

type public ElectrumServerReturningInternalErrorException(message: string, code: int,
                                                          originalRequest: string, originalResponse: string) =
    inherit ElectrumServerReturningErrorException(message, Some code, originalRequest, originalResponse)

module StratumRequestSerializer =

    let private Serialize(req: Request): string =
        JsonConvert.SerializeObject(req, Formatting.None,
                                    Marshalling.PascalCase2LowercasePlusUnderscoreConversionSettings)

    let BlockchainScriptHashGetBalance (address: string): string =
        let req = {
            Id = 0
            Method = "blockchain.scripthash.get_balance"
            Params = address :: List.Empty
        }
        Serialize req

    let ServerVersion (clientName: string) (protocolVersion: Version): string =
        let req = {
            Id = 0
            Method = "server.version"
            Params = [clientName; protocolVersion.ToString()]
        }
        Serialize req

    let BlockchainScriptHashListUnspent (address: string): string =
        let req = {
            Id = 0
            Method = "blockchain.scripthash.listunspent"
            Params = address :: List.Empty
        }
        Serialize req

    let BlockchainScriptHashHistory (scriptHash: string): string =
        let req = {
            Id = 0
            Method = "blockchain.scripthash.get_history"
            Params = scriptHash :: List.Empty
        }
        Serialize req

    let BlockchainScriptHashMerkle (txHash: string) height: string =
        let req = {
            Id = 0
            Method = "blockchain.transaction.get_merkle"
            Params = [txHash :> obj; height :> obj]
        }
        Serialize req

    let BlockchainTransactionGet (txHash: string): string=
        let req = {
            Id = 0
            Method = "blockchain.transaction.get"
            Params = txHash :: List.Empty
        }
        Serialize req

    let BlockchainTransactionGetVerbose (txHash: string): string =
        let req = {
            Id = 0
            Method = "blockchain.transaction.get"
            Params = [txHash :> obj; true :> obj]
        }
        Serialize req

    let BlockchainEstimateFee (numBlocksTarget: int): string =
        let req = {
            Id = 0;
            Method = "blockchain.estimatefee";
            Params = numBlocksTarget :: List.Empty
        }
        Serialize req

    let BlockchainTransactionBroadcast (txInHex: string): string =
        let req = {
            Id = 0
            Method = "blockchain.transaction.broadcast"
            Params = txInHex :: List.Empty
        }
        Serialize req

    let BlockchainHeadersSubscribe (): string =
        let req = {
            Id = 0
            Method = "blockchain.headers.subscribe"
            Params = List.Empty
        }
        Serialize req

type StratumClient (jsonRpcClient: JsonRpcTcpClient) =

    // FIXME: should we rather use JContainer.Parse? it seems JObject.Parse wouldn't detect error in this: {A:{"B": 1}}
    //        (for more info see replies of https://stackoverflow.com/questions/6903477/need-a-string-json-validator )
    static member private IsValidJson (jsonStr: string) =
        try
            Newtonsoft.Json.Linq.JObject.Parse jsonStr
                |> ignore
            true
        with
        | :? JsonReaderException ->
            false

    // TODO: add 'T as incoming request type, leave 'R as outgoing response type
    member private self.Request<'R> (jsonRequest: string): Async<'R*string> = async {
        let! rawResponse = jsonRpcClient.Request jsonRequest

        // FIXME: we should actually fix this bug in JsonRpcSharp (https://github.com/nblockchain/JsonRpcSharp/issues/9)
        if String.IsNullOrEmpty rawResponse then
            return raise <| ProtocolGlitchException(SPrintF2 "Server '%s' returned a null/empty JSON response to the request '%s'??"
                                                             jsonRpcClient.Host jsonRequest)

        try
            return (StratumClient.Deserialize<'R> rawResponse, rawResponse)
        with
        | :? ElectrumServerReturningErrorInJsonResponseException as ex ->
            match ex.ErrorCode with
            | Some errorCode when errorCode = int RpcErrorCode.InternalError ->
                return raise
                <| ElectrumServerReturningInternalErrorException(ex.Message, errorCode, jsonRequest, rawResponse)
            | Some errorCode when errorCode = int RpcErrorCode.UnknownMethod ->
                return raise <| ServerMisconfiguredException(ex.Message, ex)
            | Some errorCode when errorCode = int RpcErrorCode.ServerBusy ->
                return raise <| ServerUnavailabilityException(ex.Message, ex)
            | Some errorCode when errorCode = int RpcErrorCode.ExcessiveResourceUsage ->
                return raise <| ServerUnavailabilityException(ex.Message, ex)
            | Some errorCode ->
                return raise
                <| ElectrumServerReturningErrorException(ex.Message, Some errorCode, jsonRequest, rawResponse)
            | None ->
                return raise
                <| ElectrumServerReturningErrorException(ex.Message, None, jsonRequest, rawResponse)
    }

    static member private MaybeDeserializeToErrorAndThrow<'T> (resultTrimmed: string): unit =
        let maybeError =
            try
                JsonConvert.DeserializeObject<ErrorResult>(resultTrimmed,
                                                           Marshalling.PascalCase2LowercasePlusUnderscoreConversionSettings)
            with
            | ex -> raise <| Exception(SPrintF2 "Failed deserializing JSON response (to check for error) '%s' to type '%s'"
                                               resultTrimmed typedefof<'T>.FullName, ex)

        if (not (Object.ReferenceEquals(maybeError, null))) && (not (Object.ReferenceEquals(maybeError.Error, null))) then
            raise
            <| ElectrumServerReturningErrorInJsonResponseException(maybeError.Error.Message, Some maybeError.Error.Code)


    static member private DeserializeInternal<'T> (result: string): 'T =
        let resultTrimmed = result.Trim()

        StratumClient.MaybeDeserializeToErrorAndThrow<'T> resultTrimmed

        let failedDeserMsg = SPrintF2 "Failed deserializing JSON response '%s' to type '%s'"
                                      resultTrimmed typedefof<'T>.FullName
        let deserializedValue =
            try
                JsonConvert.DeserializeObject<'T>(resultTrimmed,
                                                  Marshalling.PascalCase2LowercasePlusUnderscoreConversionSettings)
            with
            | :? Newtonsoft.Json.JsonSerializationException as serEx ->
                let newEx = ElectrumServerReturningImproperJsonResponseException(failedDeserMsg, serEx)
#if !DEBUG
                Infrastructure.ReportWarning newEx
                |> ignore<bool>
#endif
                raise newEx
            | ex -> raise <| Exception(failedDeserMsg, ex)

        if Object.ReferenceEquals(deserializedValue, null) then
            failwith <| SPrintF2 "Failed deserializing JSON response '%s' to type '%s' (result was null)"
                      resultTrimmed typedefof<'T>.FullName

        deserializedValue

    // TODO: should this validation actually be part of JsonRpcSharp?
    static member public Deserialize<'T> (result: string): 'T =
        match StratumClient.IsValidJson result with
        | false ->
            raise <| ServerMisconfiguredException(SPrintF1 "Server's reply was not valid json: %s" result)
        | true ->
            StratumClient.DeserializeInternal result

    member self.BlockchainScriptHashGetBalance address: Async<BlockchainScriptHashGetBalanceResult> =
        let json = StratumRequestSerializer.BlockchainScriptHashGetBalance address

        async {
            let! resObj,_ = self.Request<BlockchainScriptHashGetBalanceResult> json
            return resObj
        }

    static member private CreateVersion(versionStr: string): Version =
        let correctedVersion =
            if (versionStr.EndsWith("+")) then
                versionStr.Substring(0, versionStr.Length - 1)
            else
                versionStr
        try
            Version(correctedVersion)
        with
        | exn -> raise(Exception("Electrum Server's version disliked by .NET Version class: " + versionStr, exn))

    member self.ServerVersion (clientName: string) (protocolVersion: Version): Async<Version> = async {
        // this below serializes to:
        //  (SPrintF2 "{ \"id\": 0, \"method\": \"server.version\", \"params\": [ \"%s\", \"%s\" ] }"
        //      CURRENT_ELECTRUM_FAKED_VERSION PROTOCOL_VERSION)
        let json = StratumRequestSerializer.ServerVersion clientName protocolVersion

        let! resObj, rawResponse = self.Request<ServerVersionResult> json

        if Object.ReferenceEquals (resObj, null) then
            failwith <| SPrintF1 "resObj is null?? raw response was %s" rawResponse

        if Object.ReferenceEquals (resObj.Result, null) then
            failwith <| SPrintF1 "resObj.Result is null?? raw response was %s" rawResponse

        // resObj.Result.[0] is e.g. "ElectrumX 1.4.3"
        // e.g. "1.1"
        let serverProtocolVersion = resObj.Result.[1]

        return StratumClient.CreateVersion(serverProtocolVersion)
    }

    member self.BlockchainScriptHashListUnspent address: Async<BlockchainScriptHashListUnspentResult> =
        let json = StratumRequestSerializer.BlockchainScriptHashListUnspent address
        async {
            let! resObj,_ = self.Request<BlockchainScriptHashListUnspentResult> json
            return resObj
        }

    member self.BlockchainScriptHashHistory scriptHash: Async<BlockchainScriptHashHistoryResult> =
        let json = StratumRequestSerializer.BlockchainScriptHashHistory scriptHash
        async {
            let! resObj,_ = self.Request<BlockchainScriptHashHistoryResult> json
            return resObj
        }

    member self.BlockchainScriptHashMerkle txHash height: Async<BlockchainScriptHashMerkleResult> =
        let json = StratumRequestSerializer.BlockchainScriptHashMerkle txHash height
        async {
            let! resObj,_ = self.Request<BlockchainScriptHashMerkleResult> json
            return resObj
        }

    member self.BlockchainTransactionGet txHash: Async<BlockchainTransactionGetResult> =
        let json = StratumRequestSerializer.BlockchainTransactionGet txHash
        async {
            let! resObj,_ = self.Request<BlockchainTransactionGetResult> json
            return resObj
        }

    member self.BlockchainTransactionGetVerbose (txHash: string): Async<BlockchainTransactionGetVerboseResult> =
        let json = StratumRequestSerializer.BlockchainTransactionGetVerbose txHash
        async {
            let! resObj,_ = self.Request<BlockchainTransactionGetVerboseResult> json
            return resObj
        }

    // NOTE: despite Electrum-X official docs claiming that this method is deprecated... it's not! go read the official
    //       non-shitcoin forked version of the docs: https://electrumx-spesmilo.readthedocs.io/en/latest/protocol-methods.html#blockchain-estimatefee
    member self.BlockchainEstimateFee (numBlocksTarget: int): Async<BlockchainEstimateFeeResult> =
        let json = StratumRequestSerializer.BlockchainEstimateFee numBlocksTarget

        async {
            let! resObj,_ = self.Request<BlockchainEstimateFeeResult> json
            return resObj
        }

    member self.BlockchainTransactionBroadcast txInHex: Async<BlockchainTransactionBroadcastResult> =
        let json = StratumRequestSerializer.BlockchainTransactionBroadcast txInHex

        async {
            let! resObj,_ = self.Request<BlockchainTransactionBroadcastResult> json
            return resObj
        }

    member self.BlockchainHeadersSubscribe (): Async<BlockchainHeadersSubscribeResult> =
        let json = StratumRequestSerializer.BlockchainHeadersSubscribe ()

        async {
            let! resObj,_ = self.Request<BlockchainHeadersSubscribeResult> json
            return resObj
        }
