namespace GWallet.Backend

open System

type IBlockchainFeeInfo =
    abstract FeeEstimationTime : DateTime
    abstract FeeValue : decimal
    abstract Currency : Currency
