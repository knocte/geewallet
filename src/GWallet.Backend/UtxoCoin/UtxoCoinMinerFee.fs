namespace GWallet.Backend.UtxoCoin

open System

open GWallet.Backend

//FIXME: convert to record?
type MinerFee
    (
        estimatedFeeInSatoshis: int64,
        estimationTime: DateTime,
        currency: Currency
    ) =

    member val EstimatedFeeInSatoshis = estimatedFeeInSatoshis

    member val EstimationTime = estimationTime

    member val Currency = currency
