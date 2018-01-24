﻿namespace GWallet.Backend.Ether

open System
open System.Net
open System.Numerics
open System.Threading.Tasks

open Nethereum.Hex.HexTypes
open Nethereum.Web3
open Nethereum.RPC.Eth.DTOs

open GWallet.Backend

module Server =

    type ConnectionUnsuccessfulException =
        inherit Exception

        new(message: string, innerException: Exception) = { inherit Exception(message, innerException) }
        new(message: string) = { inherit Exception(message) }

    type ServerTimedOutException(message:string) =
       inherit ConnectionUnsuccessfulException (message)

    type ServerCannotBeResolvedException(message:string, innerException: Exception) =
       inherit ConnectionUnsuccessfulException (message, innerException)

    type IWeb3 =
        abstract member GetTransactionCount: string -> Task<HexBigInteger>
        abstract member GetUnconfirmedBalance: string -> Task<HexBigInteger>
        abstract member GetConfirmedBalance: string -> Task<HexBigInteger>
        abstract member GetGasPrice: unit -> Task<HexBigInteger>
        abstract member BroadcastTransaction: string -> Task<string>

    type SomeWeb3(web3: Web3) =
        let NUMBER_OF_CONFIRMATIONS_TO_CONSIDER_BALANCE_CONFIRMED = BigInteger(45)

        interface IWeb3 with
            member this.GetTransactionCount (publicAddress): Task<HexBigInteger> =
                web3.Eth.Transactions.GetTransactionCount.SendRequestAsync
                    publicAddress
            member this.GetUnconfirmedBalance (publicAddress): Task<HexBigInteger> =
                web3.Eth.GetBalance.SendRequestAsync
                    publicAddress
            member this.GetConfirmedBalance (publicAddress): Task<HexBigInteger> =
                Task.Run(fun _ ->
                    let latestBlockTask = web3.Eth.Blocks.GetBlockNumber.SendRequestAsync ()
                    latestBlockTask.Wait()
                    let latestBlock = latestBlockTask.Result
                    let blockForConfirmationReference =
                        BlockParameter(HexBigInteger(BigInteger.Subtract(latestBlock.Value,
                                                                         NUMBER_OF_CONFIRMATIONS_TO_CONSIDER_BALANCE_CONFIRMED)))
(*
                    if (Config.DebugLog) then
                        Console.Error.WriteLine (sprintf "Last block number and last confirmed block number: %s: %s"
                                                         (latestBlock.Value.ToString()) (blockForConfirmationReference.BlockNumber.Value.ToString()))
*)
                    let balanceTask =
                        web3.Eth.GetBalance.SendRequestAsync(publicAddress,blockForConfirmationReference)
                    balanceTask.Wait()
                    balanceTask.Result
                )
            member this.GetGasPrice (): Task<HexBigInteger> =
                web3.Eth.GasPrice.SendRequestAsync ()
            member this.BroadcastTransaction transaction: Task<string> =
                web3.Eth.Transactions.SendRawTransaction.SendRequestAsync transaction

    //let private PUBLIC_WEB3_API_ETH_INFURA = "https://mainnet.infura.io:8545" ?
    let private PUBLIC_WEB3_API_ETH_INFURA_MEW = "https://mainnet.infura.io/mew"
    let private PUBLIC_WEB3_API_ETH_MEW = "https://api.myetherapi.com/eth" // docs: https://www.myetherapi.com/

    // this below is https://classicetherwallet.com/'s public endpoint (TODO: to prevent having a SPOF, use https://etcchain.com/api/ too)
    let private PUBLIC_WEB3_API_ETC = "https://mewapi.epool.io"

    let private ethWeb3Infura = SomeWeb3(Web3(PUBLIC_WEB3_API_ETH_INFURA_MEW)):>IWeb3
    let private ethWeb3Mew = SomeWeb3(Web3(PUBLIC_WEB3_API_ETH_MEW)):>IWeb3
    let private etcWeb3 = SomeWeb3(Web3(PUBLIC_WEB3_API_ETC)):>IWeb3

    let GetWeb3Servers (currency: Currency): list<IWeb3> =
        match currency with
        | Currency.ETC ->
            [ etcWeb3 ]
        | Currency.ETH ->
            [ ethWeb3Infura; ethWeb3Mew ]


    let exMsg = "Could not communicate with EtherServer"
    let WaitOnTask<'T,'R> (func: 'T -> Task<'R>) (arg: 'T) =
        let task = func arg
        let finished =
            try
                task.Wait Config.DEFAULT_NETWORK_TIMEOUT
            with
            | ex ->
                let maybeWebEx = FSharpUtil.FindException<WebException> ex
                match maybeWebEx with
                | None -> reraise()
                | Some(webEx) ->
                    if (webEx.Status = WebExceptionStatus.NameResolutionFailure) then
                        raise (ServerCannotBeResolvedException(exMsg, webEx))
                    reraise()
        if not finished then
            raise (ServerTimedOutException(exMsg))
        task.Result

    // we only have infura and mew for now, so requiring more than 1 would make it not fault tolerant...:
    let private NUMBER_OF_CONSISTENT_RESPONSES_TO_TRUST_ETH_SERVER_RESULTS = 1

    let private faultTolerantEthClient =
        FaultTolerantClient<ConnectionUnsuccessfulException> NUMBER_OF_CONSISTENT_RESPONSES_TO_TRUST_ETH_SERVER_RESULTS

    // FIXME: there should be a way to simplify this function to be more readable...
    //        maybe make it more similar to BitcoinAccount.fs's GetRandomizedFuncs()?
    let private PlumbingCall<'T,'R when 'R:equality> (currency: Currency)
                                    (arg: 'T)
                                    (web3Func: IWeb3 -> ('T -> Task<'R>))
                                    : 'R =
        let web3s = GetWeb3Servers currency
        let funcs =
            List.map (fun (web3: IWeb3) ->
                          fun (arg1: 'T) ->
                              WaitOnTask (fun (arg11:'T) -> web3Func web3 arg11) arg1)
                      web3s
        faultTolerantEthClient.Query<'T,'R> arg funcs

    let GetTransactionCount (currency: Currency) (address: string)
        : HexBigInteger =
        PlumbingCall currency address (fun web3 -> web3.GetTransactionCount)

    let GetUnconfirmedBalance (currency: Currency) (address: string)
        : HexBigInteger =
        PlumbingCall currency address (fun web3 -> web3.GetUnconfirmedBalance)

    let GetConfirmedBalance (currency: Currency) (address: string)
        : HexBigInteger =
        PlumbingCall currency address (fun web3 -> web3.GetConfirmedBalance)

    let GetGasPrice (currency: Currency)
        : HexBigInteger =
        PlumbingCall currency () (fun web3 -> web3.GetGasPrice)

    let BroadcastTransaction (currency: Currency) transaction
        : string =
        PlumbingCall currency transaction (fun web3 -> web3.BroadcastTransaction)
