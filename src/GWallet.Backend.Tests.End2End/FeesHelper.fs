namespace GWallet.Backend.Tests.End2End

open System

open NBitcoin
open DotNetLightning.Utils

open GWallet.Backend
open GWallet.Backend.UtxoCoin

module FeesHelper =

    let GetFeeFromTransaction (tx: Transaction): Async<Money> = async {
        let rec sumInputs (acc: Money) (inputs: seq<TxIn>): Async<Money> = async {
            match Seq.tryHead inputs with
            | None -> return acc
            | Some input ->
                let outpoint = input.PrevOut
                let! parentTxString =
                    Server.Query
                        Currency.BTC
                        (QuerySettings.Default ServerSelectionMode.Fast)
                        (ElectrumClient.GetBlockchainTransaction (outpoint.Hash.ToString()))
                        None
                let parentTx = Transaction.Parse(parentTxString, Network.RegTest)
                let value = parentTx.Outputs.[outpoint.N].Value
                return! sumInputs (acc + value) (Seq.tail inputs)
        }
        let rec sumOutputs (acc: Money) (outputs: seq<TxOut>): Money =
            match Seq.tryHead outputs with
            | None -> acc
            | Some output ->
                sumOutputs (acc + output.Value) (Seq.tail outputs)
        let! inputValue = sumInputs Money.Zero tx.Inputs
        let outputValue = sumOutputs Money.Zero tx.Outputs
        return inputValue - outputValue
    }

    let FeeRatesApproxEqual (x: FeeRatePerKw) (y: FeeRatePerKw): bool =
        Math.Abs(Math.Log(double x.Value) - Math.Log(double y.Value)) < 0.1
