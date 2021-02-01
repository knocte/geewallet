namespace GWallet.Backend.UtxoCoin

open System

open GWallet.Backend
open GWallet.Backend.FSharpUtil.UwpHacks

module ElectrumClient =

    let private Init (fqdn: string) (port: uint32): Async<StratumClient> =
        let jsonRpcClient = new JsonRpcTcpClient(fqdn, port)
        let stratumClient = new StratumClient(jsonRpcClient)

        // this is the last version of Electrum released at the time of writing this module
        let CLIENT_NAME_SENT_TO_STRATUM_SERVER_WHEN_HELLO = "geewallet"

        // last version of the protocol [1] as of electrum's source code [2] at the time of
        // writing this... actually this changes relatively rarely (one of the last changes
        // was for 2.4 version [3] (changes documented here[4])
        // [1] https://electrumx-spesmilo.readthedocs.io/en/latest/protocol.html
        // [2] https://github.com/spesmilo/electrum/blob/master/lib/version.py
        // [3] https://github.com/spesmilo/electrum/commit/118052d81597eff3eb636d242eacdd0437dabdd6
        // [4] https://electrumx-spesmilo.readthedocs.io/en/latest/protocol-changes.html
        let PROTOCOL_VERSION_SUPPORTED = Version "1.4"

        async {
            let! versionSupportedByServer =
                try
                    stratumClient.ServerVersion CLIENT_NAME_SENT_TO_STRATUM_SERVER_WHEN_HELLO PROTOCOL_VERSION_SUPPORTED
                with
                | :? ElectrumServerReturningErrorException as ex ->
                    if (ex.ErrorCode = 1 && ex.Message.StartsWith "unsupported protocol version" &&
                                            ex.Message.EndsWith (PROTOCOL_VERSION_SUPPORTED.ToString())) then

                        // FIXME: even if this ex is already handled to ignore the server, we should report to sentry as WARN
                        raise <| ServerTooNewException(SPrintF1 "Version of server rejects our client version (%s)"
                                                             (PROTOCOL_VERSION_SUPPORTED.ToString()))
                    else
                        reraise()
            if versionSupportedByServer < PROTOCOL_VERSION_SUPPORTED then
                raise (ServerTooOldException (SPrintF2 "Version of server is older (%s) than the client (%s)"
                                                      (versionSupportedByServer.ToString())
                                                      (PROTOCOL_VERSION_SUPPORTED.ToString())))
            return stratumClient
        }

    let StratumServer (electrumServer: ServerDetails): Async<StratumClient> =
        match electrumServer.ServerInfo.ConnectionType with
        | { Encrypted = true; Protocol = _ } -> failwith "Incompatibility filter for non-encryption didn't work?"
        | { Encrypted = false; Protocol = Http } -> failwith "HTTP server for UtxoCoin?"
        | { Encrypted = false; Protocol = Tcp port } ->
            Init electrumServer.ServerInfo.NetworkPath port

    let GetBalance (scriptHash: string) (stratumServer: Async<StratumClient>) = async {
        // FIXME: we should rather implement this method in terms of:
        //        - querying all unspent transaction outputs (X) -> block heights included
        //        - querying transaction history (Y) -> block heights included
        //        - check the difference between X and Y (e.g. Y - X = Z)
        //        - query details of each element in Z to see their block heights
        //        - query the current blockheight (H) -> pick the highest among all servers queried
        //        -> having H, we now know which elements of X, Y, and Z are confirmed or not
        // Doing it this way has two advantages:
        // 1) We can configure GWallet with a number of confirmations to consider some balance confirmed (instead
        //    of trusting what "confirmed" means from the point of view of the Electrum Server)
        // 2) and most importantly: we could verify each of the transactions supplied in X, Y, Z to verify their
        //    integrity (in a similar fashion as Electrum Wallet client already does), to not have to trust servers*
        //    [ see https://www.youtube.com/watch?v=hjYCXOyDy7Y&feature=youtu.be&t=1171 for more information ]
        // * -> although that would be fixing only half of the problem, we also need proof of completeness
        let! stratumClient = stratumServer
        let! balanceResult = stratumClient.BlockchainScriptHashGetBalance scriptHash
        return balanceResult.Result
    }

    let GetUnspentTransactionOutputs scriptHash (stratumServer: Async<StratumClient>) = async {
        let! stratumClient = stratumServer
        let! unspentListResult = stratumClient.BlockchainScriptHashListUnspent scriptHash
        return unspentListResult.Result
    }

    let GetBlockchainScriptHashHistory scriptHash (stratumServer: Async<StratumClient>) = async {
        let! stratumClient = stratumServer
        let! history = stratumClient.BlockchainScriptHashHistory scriptHash
        return history.Result
    }

    let GetBlockchainScriptHashMerkle txHash height (stratumServer: Async<StratumClient>) = async {
        let! stratumClient = stratumServer
        let! history = stratumClient.BlockchainScriptHashMerkle txHash height
        return history.Result
    }

    let GetBlockchainTransaction txHash (stratumServer: Async<StratumClient>) = async {
        let! stratumClient = stratumServer
        let! blockchainTransactionResult = stratumClient.BlockchainTransactionGet txHash
        return blockchainTransactionResult.Result
    }

    let GetBlockchainTransactionVerbose (txHash: string) (stratumServer: Async<StratumClient>) = async {
        let! stratumClient = stratumServer
        let! blockchainTransactionResult = stratumClient.BlockchainTransactionGetVerbose txHash
        return blockchainTransactionResult.Result
    }

    let GetConfirmations (txHash: string)
                         (stratumServer: Async<StratumClient>)
                            : Async<uint32> = async {
        let! verboseTransactionInfoOpt = async {
            try
                let! stratumClient = stratumServer
                let! queryResult = stratumClient.BlockchainTransactionGetVerbose txHash
                return Some queryResult
            with
            | :? ElectrumServerReturningErrorInJsonResponseException as ex ->

                // NOTE: The response returned by the electrum servers when the
                // transaction doesn't exist is:
                //
                // {"jsonrpc": "2.0", "error": {"code": 2, "message": "daemon error: DaemonError({'code': -5, 'message': 'No such mempool or blockchain transaction. Use gettransaction for wallet transactions.'})"}, "id": 0}

                let unknownTransactionOuterErrorCode = 2
                let unknownTransactionInnerErrorCode = -5
                if ex.ErrorCode = unknownTransactionOuterErrorCode then
                    let innerErrorCodeFound =
                        Seq.exists
                            (fun (ex: ElectrumServerReturningErrorInJsonResponseException) -> ex.ErrorCode = unknownTransactionInnerErrorCode)
                            (ex.InternalErrors())
                    if innerErrorCodeFound then
                        return None
                    else
                        return raise <| FSharpUtil.ReRaise ex
                else
                    return raise <| FSharpUtil.ReRaise ex
        }
        match verboseTransactionInfoOpt with
        | None -> return 0u
        | Some verboseTransactionInfo ->
            return verboseTransactionInfo.Result.Confirmations |> uint32
    }

    let private EstimateFeeInternal
        (numBlocksTarget: int)
        (stratumServer: Async<StratumClient>)
        : Async<decimal> = async {
        let! stratumClient = stratumServer
        let! estimateFeeResult = stratumClient.BlockchainEstimateFee numBlocksTarget
        if estimateFeeResult.Result = -1m then
            return raise <| ServerMisconfiguredException("Fee estimation returned a -1 error code")
        elif estimateFeeResult.Result <= 0m then
            return raise <| ServerMisconfiguredException(SPrintF1 "Fee estimation returned an invalid non-positive value %M"
                                                                  estimateFeeResult.Result)

        let amountPerKB = estimateFeeResult.Result
        let satPerKB = (NBitcoin.Money (amountPerKB, NBitcoin.MoneyUnit.BTC)).ToUnit NBitcoin.MoneyUnit.Satoshi
        let satPerB = satPerKB / (decimal 1000)
        Infrastructure.LogDebug <| SPrintF2
            "Electrum server gave us a fee rate of %M per KB = %M sat per B" amountPerKB satPerB
        return amountPerKB
    }

    let EstimateFee (numBlocksTarget: int) (stratumServer: Async<StratumClient>): Async<decimal> =
        if Config.BitcoinNet() = NBitcoin.Network.RegTest then
            // when running in regtest mode we don't contact the electrum server to request the fee but instead return
            // a hard-coded value. This is because electrum gets its fee rate from bitcoind but bitcoind is usually not
            // able to estimate fee rates in regtest mode, so the electrum request would fail
            async { return 0.0001m }
        else
            EstimateFeeInternal numBlocksTarget stratumServer

    let BroadcastTransaction (transactionInHex: string) (stratumServer: Async<StratumClient>) = async {
        let! stratumClient = stratumServer
        let! blockchainTransactionBroadcastResult = stratumClient.BlockchainTransactionBroadcast transactionInHex
        return blockchainTransactionBroadcastResult.Result
    }

    let SubscribeHeaders () (stratumServer: Async<StratumClient>): Async<BlockchainHeadersSubscribeInnerResult> = async {
        let! stratumClient = stratumServer
        let! blockchainHeadersSubscribeResult = stratumClient.BlockchainHeadersSubscribe ()
        return blockchainHeadersSubscribeResult.Result
    }
