namespace GWallet.Backend

open System

open GWallet.Backend
open GWallet.Backend.Ether
open GWallet.Backend.FSharpUtil.UwpHacks

module BlockExplorer =

    let GetTransactionHistory (account: IAccount) : Uri =
        let baseUrl =
            match account.Currency with
            | Currency.BTC ->
                // SmartBit explorer is built on top of NBitcoin: https://github.com/ProgrammingBlockchain/ProgrammingBlockchain/issues/1
                "https://www.smartbit.com.au/address/"
            | Currency.LTC ->
                // because the more popular https://live.blockcypher.com/ltc/ doesn't seem to have segwit support
                "https://chainz.cryptoid.info/ltc/address.dws?"
            | Currency.ETH ->
                // most popular one...
                "https://etherscan.io/address/"
            | Currency.ETC ->
                // maybe blockscout is better? minergate.com seems to only show blocks, not addresses
                "https://etcblockexplorer.com/address/addr/"
            | Currency.SAI
            | Currency.DAI ->
                SPrintF1
                    "https://etherscan.io/token/%s?a="
                    (TokenManager.GetTokenContractAddress account.Currency)

        Uri (baseUrl + account.PublicAddress)

    let GetTransaction (currency: Currency) (txHash: string) : Uri =
        let baseUrl =
            match currency with
            | Currency.BTC -> "https://www.smartbit.com.au/tx/"
            | Currency.LTC -> "https://chainz.cryptoid.info/ltc/tx.dws?"
            | Currency.ETH -> "https://etherscan.io/tx/"
            | Currency.ETC -> "https://etcblockexplorer.com/tx/"
            | Currency.DAI
            | Currency.SAI -> "https://etherscan.io/tx/"

        Uri (baseUrl + txHash)
