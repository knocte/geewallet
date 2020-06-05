namespace GWallet.Backend.UtxoCoin.Lightning

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Chain

open GWallet.Backend.UtxoCoin

type internal FeeEstimator =
    {
        FeeRatePerKw: FeeRatePerKw
    }
    static member Create currency = async {
        let averageFee (feesFromDifferentServers: List<decimal>): decimal =
            let sum: decimal = List.sum feesFromDifferentServers
            let avg = sum / decimal feesFromDifferentServers.Length
            avg
        let estimateFeeJob = ElectrumClient.EstimateFee Account.CONFIRMATION_BLOCK_TARGET
        let! btcPerKB = Server.Query currency (QuerySettings.FeeEstimation averageFee) estimateFeeJob None
        let satPerKB = (Money (btcPerKB, MoneyUnit.BTC)).ToUnit MoneyUnit.Satoshi
        // 4 weight units per byte. See segwit specs.
        let kwPerKB = 4m
        let satPerKw = satPerKB / kwPerKB
        let feeRatePerKw = FeeRatePerKw (uint32 satPerKw)
        return { FeeRatePerKw = feeRatePerKw }
    }

    interface IFeeEstimator with
        member self.GetEstSatPer1000Weight(_: ConfirmationTarget) =
            self.FeeRatePerKw
