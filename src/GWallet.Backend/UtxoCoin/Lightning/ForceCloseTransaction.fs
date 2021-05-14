namespace GWallet.Backend.UtxoCoin.Lightning

open System.Linq

open NBitcoin
open DotNetLightning.Channel
open DotNetLightning.Utils

open GWallet.Backend
open GWallet.Backend.UtxoCoin

module public ForceCloseTransaction =

    let internal CreatePunishmentTx (perCommitmentSecret: PerCommitmentSecret)
                                    (commitments: Commitments)
                                    (localChannelPrivKeys: ChannelPrivKeys)
                                    (network: Network)
                                    (account: NormalUtxoAccount)
                                        : Async<Transaction> =
        async {
            let transactionBuilder =
                ForceCloseFundsRecovery.createPenaltyTx
                    commitments.LocalParams
                    commitments.RemoteParams
                    perCommitmentSecret
                    commitments.RemoteCommit
                    localChannelPrivKeys
                    network

            let targetAddress =
                let originAddress = (account :> IAccount).PublicAddress
                BitcoinAddress.Create(originAddress, network)

            transactionBuilder.SendAll targetAddress
            |> ignore

            let! btcPerKiloByteForFastTrans =
                let averageFee (feesFromDifferentServers: List<decimal>): decimal =
                    feesFromDifferentServers.Sum()
                    / decimal feesFromDifferentServers.Length

                let estimateFeeJob =
                    ElectrumClient.EstimateFee Account.CONFIRMATION_BLOCK_TARGET

                Server.Query (account :> IAccount).Currency (QuerySettings.FeeEstimation averageFee) estimateFeeJob None

            let fee =
                let feeRate =
                    Money(btcPerKiloByteForFastTrans, MoneyUnit.BTC)
                    |> FeeRate

                transactionBuilder.EstimateFees feeRate

            transactionBuilder.SendFees fee |> ignore

            return transactionBuilder.BuildTransaction true
        }
