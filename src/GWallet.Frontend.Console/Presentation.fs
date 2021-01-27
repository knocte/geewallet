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

    let ShowFee (transactionCurrency: Currency) (estimatedFee: IBlockchainFeeInfo) =
        let currency = estimatedFee.Currency
        let estimatedFeeInUsd =
            match FiatValueEstimation.UsdValue(currency) |> Async.RunSynchronously with
            | Fresh usdValue ->
                sprintf "(~%s USD)"
                    (usdValue * estimatedFee.FeeValue |> Formatting.DecimalAmountRounding CurrencyType.Fiat)
            | NotFresh(Cached(usdValue,time)) ->
                sprintf "(~%s USD [last known rate at %s])"
                    (usdValue * estimatedFee.FeeValue |> Formatting.DecimalAmountRounding CurrencyType.Fiat)
                    (time |> Formatting.ShowSaneDate)
            | NotFresh NotAvailable -> ExchangeRateUnreachableMsg
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

    let ShowTransactionData<'T when 'T:> IBlockchainFeeInfo> (trans: UnsignedTransaction<'T>) =
        let maybeUsdPrice = FiatValueEstimation.UsdValue(trans.Proposal.Amount.Currency)
                            |> Async.RunSynchronously
        let maybeEstimatedAmountInUsd: Option<string> =
            match maybeUsdPrice with
            | Fresh usdPrice ->
                Some(sprintf "~ %s USD"
                             (trans.Proposal.Amount.ValueToSend * usdPrice
                                 |> Formatting.DecimalAmountRounding CurrencyType.Fiat))
            | NotFresh(Cached(usdPrice, time)) ->
                Some(sprintf "~ %s USD (last exchange rate known at %s)"
                        (trans.Proposal.Amount.ValueToSend * usdPrice
                            |> Formatting.DecimalAmountRounding CurrencyType.Fiat)
                        (time |> Formatting.ShowSaneDate))
            | NotFresh NotAvailable -> None

        Console.WriteLine "Transaction data:"
        Console.WriteLine("Sender: " + trans.Proposal.OriginAddress)
        Console.WriteLine("Recipient: " + trans.Proposal.DestinationAddress)
        let fiatAmount =
            match maybeEstimatedAmountInUsd with
            | Some(estimatedAmountInUsd) -> estimatedAmountInUsd
            | _ -> String.Empty
        Console.WriteLine (sprintf "Amount: %s %A %s"
                                   (trans.Proposal.Amount.ValueToSend |> Formatting.DecimalAmountRounding CurrencyType.Crypto)
                                   trans.Proposal.Amount.Currency
                                   fiatAmount)
        Console.WriteLine()
        ShowFee trans.Proposal.Amount.Currency trans.Metadata
