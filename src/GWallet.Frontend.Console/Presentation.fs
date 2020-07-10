namespace GWallet.Frontend.Console

open System
open System.Text.RegularExpressions

open GWallet.Backend

module Presentation =

    let Error(message: string): unit =
        Console.Error.WriteLine("Error: " + message)

    let ConvertPascalCaseToSentence(pascalCaseElement: string) =
        Regex.Replace(pascalCaseElement, "[a-z][A-Z]",
                      (fun (m: Match) -> m.Value.[0].ToString() + " " + Char.ToLower(m.Value.[1]).ToString()))

    let internal ExchangeRateUnreachableMsg = " (USD exchange rate unreachable... offline?)"

    let ShowFee (maybeUsdPrice: MaybeCached<decimal>)
                (transactionCurrency: Currency)
                (estimatedFee: IBlockchainFeeInfo) =
        let currency = estimatedFee.Currency
        let estimatedFeeInUsd =
            match maybeUsdPrice with
            | Fresh(usdValue) ->
                sprintf "(~%s USD)"
                    (usdValue * estimatedFee.FeeValue |> Formatting.DecimalAmountRounding CurrencyType.Fiat)
            | NotFresh(Cached(usdValue,time)) ->
                sprintf "(~%s USD [last known rate at %s])"
                    (usdValue * estimatedFee.FeeValue |> Formatting.DecimalAmountRounding CurrencyType.Fiat)
                    (time |> Formatting.ShowSaneDate)
            | NotFresh(NotAvailable) -> ExchangeRateUnreachableMsg
        let feeMsg =
            if transactionCurrency = Currency.DAI &&
               Config.EthTokenEstimationCouldBeBuggyAsInNotAccurate then
                "Estimated fee for this transaction would be, approximately"
            else
                "Estimated fee for this transaction would be"

        Console.WriteLine(sprintf "%s:%s %s %A %s"
                              feeMsg
                              Environment.NewLine
                              (estimatedFee.FeeValue |> Formatting.DecimalAmountRounding CurrencyType.Crypto)
                              currency
                              estimatedFeeInUsd
                         )

    let ShowTransactionData<'T when 'T:> IBlockchainFeeInfo> (trans: ITransactionDetails)
                                                             (metadata: 'T) =
        let maybeUsdPrice = FiatValueEstimation.UsdValue trans.Currency
                            |> Async.RunSynchronously
        let maybeEstimatedAmountInUsd: Option<string> =
            match maybeUsdPrice with
            | Fresh(usdPrice) ->
                Some(sprintf "~ %s USD"
                             (trans.Amount * usdPrice
                                 |> Formatting.DecimalAmountRounding CurrencyType.Fiat))
            | NotFresh(Cached(usdPrice, time)) ->
                Some(sprintf "~ %s USD (last exchange rate known at %s)"
                        (trans.Amount * usdPrice
                            |> Formatting.DecimalAmountRounding CurrencyType.Fiat)
                        (time |> Formatting.ShowSaneDate))
            | NotFresh(NotAvailable) -> None

        Console.WriteLine("Transaction data:")
        Console.WriteLine("Sender: " + trans.OriginAddress)
        Console.WriteLine("Recipient: " + trans.DestinationAddress)
        let fiatAmount =
            match maybeEstimatedAmountInUsd with
            | Some(estimatedAmountInUsd) -> estimatedAmountInUsd
            | _ -> String.Empty
        Console.WriteLine (sprintf "Amount: %s %A %s"
                                   (trans.Amount |> Formatting.DecimalAmountRounding CurrencyType.Crypto)
                                   trans.Currency
                                   fiatAmount)
        Console.WriteLine()
        ShowFee maybeUsdPrice trans.Currency metadata

    let ShowFeeAndSpendableBalance (metadata: IBlockchainFeeInfo)
                                   (channelCapacity: TransferAmount) =
        let maybeUsdPrice = FiatValueEstimation.UsdValue metadata.Currency
                            |> Async.RunSynchronously
        let estimatedSpendableBalance =
            let estimatedChannelReserve = channelCapacity.ValueToSend / 10m
            let estimatedSpendableBalance =
                channelCapacity.ValueToSend - metadata.FeeValue - estimatedChannelReserve
            Math.Max(0m, estimatedSpendableBalance)
        Console.WriteLine "Estimated spendable balance of the channel would be: "
        Console.Write(
            sprintf
                " %s BTC "
                (estimatedSpendableBalance
                    |> Formatting.DecimalAmountRounding CurrencyType.Crypto)
        )
        match maybeUsdPrice with
        | Fresh usdPrice ->
            Console.WriteLine(
                sprintf
                    "(~%s USD)"
                    (estimatedSpendableBalance * usdPrice
                        |> Formatting.DecimalAmountRounding CurrencyType.Fiat)
            )
        | NotFresh(Cached(usdPrice, time)) ->
            Console.WriteLine(
                sprintf
                    "(~%s USD (last exchange rate known at %s))"
                    (estimatedSpendableBalance * usdPrice
                        |> Formatting.DecimalAmountRounding CurrencyType.Fiat)
                    (time |> Formatting.ShowSaneDate)
            )
        | NotFresh NotAvailable ->
            Console.WriteLine()
        ShowFee maybeUsdPrice Currency.BTC metadata

