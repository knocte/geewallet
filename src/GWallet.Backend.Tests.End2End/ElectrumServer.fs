namespace GWallet.Backend.Tests.End2End

open System
open System.IO
open System.Linq

open NBitcoin
open DotNetLightning.Utils

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.FSharpUtil.UwpHacks

type ElectrumServer = {
    DbDir: string
    ProcessWrapper: ProcessWrapper
} with
    interface IDisposable with
        member self.Dispose() =
            self.ProcessWrapper.Process.Kill()
            self.ProcessWrapper.WaitForExit()
            Directory.Delete(self.DbDir, true)

    static member Start(bitcoind: Bitcoind): Async<ElectrumServer> = async {
        let dbDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
        Directory.CreateDirectory dbDir |> ignore
        let processWrapper =
            ProcessWrapper.New
                "electrs"
                (SPrintF3
                    "\
                    --db-dir %s \
                    --daemon-dir %s \
                    --network regtest \
                    --electrum-rpc-addr [::1]:50001 \
                    --daemon-rpc-addr %s \
                    "
                    dbDir
                    bitcoind.DataDir
                    (bitcoind.RpcAddr())
                )
                Map.empty
                false
        processWrapper.WaitForMessage (fun msg -> msg.Contains "Electrum Rust Server")

        do! Async.Sleep 5000

        return {
            DbDir = dbDir
            ProcessWrapper = processWrapper
        }
    }

    static member EstimateFeeRate(): Async<FeeRatePerKw> = async {
        let! btcPerKB =
            let averageFee (feesFromDifferentServers: list<decimal>): decimal =
                feesFromDifferentServers.Sum() / decimal (List.length feesFromDifferentServers)
            let estimateFeeJob = ElectrumClient.EstimateFee 6
            Server.Query
                Currency.BTC
                (QuerySettings.FeeEstimation averageFee)
                estimateFeeJob
                None
        let satPerKB = (Money (btcPerKB, MoneyUnit.BTC)).ToUnit MoneyUnit.Satoshi
        // 4 weight units per byte. See segwit specs.
        let kwPerKB = 4m
        let satPerKw = satPerKB / kwPerKB
        let feeRatePerKw = FeeRatePerKw (uint32 satPerKw)
        return feeRatePerKw
    }

    static member SetEstimatedFeeRate(feeRatePerKw: FeeRatePerKw) =
        let satPerKw = decimal feeRatePerKw.Value
        let kwPerKB = 4m
        let satPerKB = satPerKw * kwPerKB
        let btcPerKB = (Money (satPerKB, MoneyUnit.Satoshi)).ToUnit MoneyUnit.BTC
        ElectrumClient.RegTestFakeFeeRate <- btcPerKB

