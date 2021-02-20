namespace GWallet.Backend

open System
open System.Net

open FSharp.Data

open GWallet.Backend.FSharpUtil.UwpHacks

module FiatValueEstimation =
    let private PERIOD_TO_CONSIDER_PRICE_STILL_FRESH = TimeSpan.FromMinutes 2.0

    type CoinCapProvider =
        JsonProvider<"""
    {
      "data": {
        "id": "bitcoin",
        "symbol": "BTC",
        "currencySymbol": "x",
        "type": "crypto",
        "rateUsd": "6444.3132749056076909"
      },
      "timestamp": 1536347871542
    }
    """>

    type PriceProvider =
        | CoinCap
        | CoinGecko

    let private QueryOnlineInternal
        currency
        (provider: PriceProvider)
        : Async<Option<string * string>> =
        async {
            use webClient = new WebClient ()

            let tickerName =
                match currency, provider with
                | Currency.BTC, _ -> "bitcoin"
                | Currency.LTC, _ -> "litecoin"
                | Currency.ETH, _
                | Currency.SAI, _ -> "ethereum"
                | Currency.ETC, _ -> "ethereum-classic"
                | Currency.DAI, PriceProvider.CoinCap -> "multi-collateral-dai"
                | Currency.DAI, _ -> "dai"

            try
                let baseUrl =
                    match provider with
                    | PriceProvider.CoinCap ->
                        SPrintF1 "https://api.coincap.io/v2/rates/%s" tickerName
                    | PriceProvider.CoinGecko ->
                        SPrintF1
                            "https://api.coingecko.com/api/v3/simple/price?ids=%s&vs_currencies=usd"
                            tickerName

                let uri = Uri baseUrl
                let task = webClient.DownloadStringTaskAsync uri
                let! res = Async.AwaitTask task
                return Some (tickerName, res)
            with ex ->
                if (FSharpUtil.FindException<WebException> ex).IsSome then
                    return None
                else
                    return raise <| FSharpUtil.ReRaise ex
        }

    let private QueryCoinCap currency =
        async {
            let! maybeJson = QueryOnlineInternal currency PriceProvider.CoinCap

            match maybeJson with
            | None -> return None
            | Some (_, json) ->
                try
                    let tickerObj = CoinCapProvider.Parse json
                    return Some tickerObj.Data.RateUsd
                with ex ->
                    if currency = ETC then
                        // interestingly this can throw in CoinCap because retreiving ethereum-classic doesn't work...
                        return None
                    else
                        return raise <| FSharpUtil.ReRaise ex
        }

    let private QueryCoinGecko currency =
        async {
            let! maybeJson =
                QueryOnlineInternal currency PriceProvider.CoinGecko

            match maybeJson with
            | None -> return None
            | Some (ticker, json) ->
                // try to parse this as an example: {"bitcoin":{"usd":7952.29}}
                let parsedJsonObj = FSharp.Data.JsonValue.Parse json

                let usdPrice =
                    match parsedJsonObj.TryGetProperty ticker with
                    | None -> failwith <| SPrintF1 "Could not pre-parse %s" json
                    | Some innerObj ->
                        match innerObj.TryGetProperty "usd" with
                        | None -> failwith <| SPrintF1 "Could not parse %s" json
                        | Some value -> value.AsDecimal ()

                return Some usdPrice
        }

    let private RetrieveOnline currency =
        async {
            let coinGeckoJob = QueryCoinGecko currency
            let coinCapJob = QueryCoinCap currency

            let bothJobs =
                FSharpUtil.AsyncExtensions.MixedParallel2
                    coinGeckoJob
                    coinCapJob

            let! maybeUsdPriceFromCoinGecko, maybeUsdPriceFromCoinCap = bothJobs

            if maybeUsdPriceFromCoinCap.IsSome && currency = Currency.ETC then
                Infrastructure.ReportWarningMessage
                    "Currency ETC can now be queried from CoinCap provider?"
                |> ignore<bool>

            let result =
                match maybeUsdPriceFromCoinGecko, maybeUsdPriceFromCoinCap with
                | None, None -> None
                | Some usdPriceFromCoinGecko, None -> Some usdPriceFromCoinGecko
                | None, Some usdPriceFromCoinCap -> Some usdPriceFromCoinCap
                | Some usdPriceFromCoinGecko, Some usdPriceFromCoinCap ->
                    let average =
                        (usdPriceFromCoinGecko + usdPriceFromCoinCap) / 2m

                    Some average

            let realResult =
                match result with
                | Some price ->
                    let realPrice =
                        if currency = Currency.SAI then
                            let ethMultiplied = price * 0.0053m
                            ethMultiplied
                        else
                            price

                    Caching.Instance.StoreLastFiatUsdPrice (currency, realPrice)
                    realPrice |> Some
                | None -> None

            return realResult
        }

    let UsdValue (currency: Currency) : Async<MaybeCached<decimal>> =
        async {
            let maybeUsdPrice =
                Caching.Instance.RetrieveLastKnownUsdPrice currency

            match maybeUsdPrice with
            | NotAvailable ->
                let! maybeOnlineUsdPrice = RetrieveOnline currency

                match maybeOnlineUsdPrice with
                | None -> return NotFresh NotAvailable
                | Some value -> return Fresh value
            | Cached (someValue, someDate) ->
                if (someDate + PERIOD_TO_CONSIDER_PRICE_STILL_FRESH) > DateTime.UtcNow then
                    return Fresh someValue
                else
                    let! maybeOnlineUsdPrice = RetrieveOnline currency

                    match maybeOnlineUsdPrice with
                    | None -> return NotFresh (Cached (someValue, someDate))
                    | Some freshValue -> return Fresh freshValue
        }
