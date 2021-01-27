namespace GWallet.Backend

open System

open GWallet.Backend.FSharpUtil.UwpHacks

type CurrencyType =
    Fiat | Crypto

module Formatting =

    // with this we want to avoid the weird default US format of starting with the month, then day, then year... sigh
    let ShowSaneDate (date: DateTime): string =
        date.ToString "dd-MMM-yyyy"

    let DecimalAmountRounding currencyType (amount: decimal): string =
        let amountOfDecimalsToShow,formattingStrategy =
            match currencyType with
            | CurrencyType.Fiat ->
                let twoDecimals = 2
                twoDecimals,SPrintF1 "N%i" twoDecimals
            | CurrencyType.Crypto ->
                let fiveDecimals = 5
                fiveDecimals,SPrintF1 "#,0.%s" (String('#', fiveDecimals))

        let rounded = Math.Round(amount, amountOfDecimalsToShow)

        if rounded = 0m && amount > 0m then
            let tiny = 1m / decimal (pown 10 amountOfDecimalsToShow)
            tiny.ToString formattingStrategy
        else
            rounded.ToString formattingStrategy

    let DecimalAmountTruncating (currencyType: CurrencyType) (amount: decimal) (maxAmount: decimal)
                                    : string =
        let amountOfDecimalsToShow =
            match currencyType with
            | CurrencyType.Fiat -> 2
            | CurrencyType.Crypto -> 5
        // https://stackoverflow.com/a/25451689/544947
        let truncated = amount - (amount % (1m / decimal (pown 10 amountOfDecimalsToShow)))
        if truncated > maxAmount then
            failwith <| SPrintF2 "how can %s be higher than %s?" (truncated.ToString()) (maxAmount.ToString())

        DecimalAmountRounding currencyType truncated
