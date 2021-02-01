namespace GWallet.Backend

open System
open System.IO
open System.Linq
open System.Net.Http

open GWallet.Backend.FSharpUtil.UwpHacks

type CachedNetworkData =
    {
        UsdPrice: Map<Currency,CachedValue<decimal>>;
        Balances: Map<Currency,Map<PublicAddress,CachedValue<decimal>>>;
        OutgoingTransactions: Map<Currency,Map<PublicAddress,Map<string,CachedValue<decimal>>>>;
    }
    static member Empty =
        {
            UsdPrice = Map.empty
            Balances = Map.empty
            OutgoingTransactions = Map.empty
        }

    static member FromDietCache (dietCache: DietCache): CachedNetworkData =
        let now = DateTime.UtcNow
        let fiatPrices =
            [ for KeyValue(currencyString, price) in dietCache.UsdPrice -> (Currency.Parse currencyString,(price,now)) ]
                |> Map.ofSeq
        let balances =
            seq {
                for KeyValue(address,currencies) in dietCache.Addresses do
                    for currencyStr in currencies do
                        match dietCache.Balances.TryFind currencyStr with
                        | None -> ()
                        | Some balance ->
                            yield (Currency.Parse currencyStr),Map.empty.Add(address,(balance,now))
            } |> Map.ofSeq
        { UsdPrice = fiatPrices; Balances = balances
          OutgoingTransactions = Map.empty; }

    member self.ToDietCache(readOnlyAccounts: seq<ReadOnlyAccount>) =
        let rec extractAddressesFromAccounts (acc: Map<PublicAddress,List<DietCurrency>>) (accounts: List<IAccount>)
            : Map<PublicAddress,List<DietCurrency>> =
                match accounts with
                | [] -> acc
                | head::tail ->
                    let existingCurrenciesForHeadAddress =
                        match acc.TryFind head.PublicAddress with
                        | None -> List.Empty
                        | Some currencies -> currencies
                    let newAcc = acc.Add(head.PublicAddress, head.Currency.ToString()::existingCurrenciesForHeadAddress)
                    extractAddressesFromAccounts newAcc tail
        let fiatPrices =
            [ for (KeyValue(currency, (price,_))) in self.UsdPrice -> currency.ToString(),price ]
                |> Map.ofSeq
        let addresses = extractAddressesFromAccounts
                            Map.empty (List.ofSeq readOnlyAccounts |> List.map (fun acc -> acc:>IAccount))
        let balances =
            seq {
                for (KeyValue(currency, currencyBalances)) in self.Balances do
                    for (KeyValue(address, (balance,_))) in currencyBalances do
                        if readOnlyAccounts.Any(fun account -> (account:>IAccount).PublicAddress = address) then
                            yield (currency.ToString(),balance)
            } |> Map.ofSeq
        { UsdPrice = fiatPrices; Addresses = addresses; Balances = balances; }

type CacheFiles =
    {
        CachedNetworkData: FileInfo
        ServerStats: FileInfo
    }

module Caching =

    let private defaultCacheFiles =
        {
            CachedNetworkData = FileInfo(Path.Combine(Config.GetCacheDir().FullName, "networkdata.json"))
            ServerStats = FileInfo(Path.Combine(Config.GetCacheDir().FullName,
                                                ServerRegistry.ServersEmbeddedResourceFileName))
        }

    let public ImportFromJson<'T> (cacheData: string): 'T =
        Marshalling.Deserialize cacheData

    let private LoadFromDiskInner (file: FileInfo): Option<string> =
        let json = File.ReadAllText file.FullName
        if String.IsNullOrWhiteSpace json then
            None
        else
            Some json

    let droppedCachedMsgWarning = "Warning: cleaning incompatible cache data found from different GWallet version"
    let private LoadFromDiskInternal<'T> (file: FileInfo): Option<'T> =
        try
            match LoadFromDiskInner file with
            | None -> None
            | Some json ->
                try
                    let deserializedJson = ImportFromJson json
                    Some deserializedJson
                with
                | :? VersionMismatchDuringDeserializationException ->
                    Infrastructure.LogError droppedCachedMsgWarning
                    None
                | :? DeserializationException ->
                    // FIXME: report a warning to sentry here...
                    Infrastructure.LogError "Warning: cleaning incompatible cache data found"
                    Infrastructure.LogDebug (SPrintF1 "JSON content: <<<%s>>>" json)
                    None
        with
        | :? FileNotFoundException -> None

    // this weird thing could happen because the previous version of GWallet didn't have a new element
    // FIXME: we should save each Map<> into its own file
    let private WeirdNullCheckToDetectVersionConflicts x =
        Object.ReferenceEquals(x, null)

    let private LoadFromDisk (files: CacheFiles): bool*CachedNetworkData*ServerRanking =
        let maybeNetworkData = LoadFromDiskInternal<CachedNetworkData> files.CachedNetworkData
        let maybeFirstRun,resultingNetworkData =
            match maybeNetworkData with
            | None ->
                true,CachedNetworkData.Empty
            | Some networkData ->
                if WeirdNullCheckToDetectVersionConflicts networkData.OutgoingTransactions then
                    Infrastructure.LogError droppedCachedMsgWarning
                    true,CachedNetworkData.Empty
                else
                    false,networkData

        let maybeServerStats = LoadFromDiskInternal<ServerRanking> files.ServerStats
        match maybeServerStats with
        | None ->
            maybeFirstRun,resultingNetworkData,Map.empty
        | Some serverStats ->
            false,resultingNetworkData,serverStats

    let rec private MergeRatesInternal (oldMap: Map<'K, CachedValue<'V>>)
                                       (newMap: Map<'K, CachedValue<'V>>)
                                       (currencyList: List<'K>)
                                       (accumulator: Map<'K, CachedValue<'V>>) =
        match currencyList with
        | [] -> accumulator
        | address::tail ->
            let maybeCachedBalance = Map.tryFind address oldMap
            match maybeCachedBalance with
            | None ->
                let newCachedBalance = newMap.[address]
                let newAcc = accumulator.Add(address, newCachedBalance)
                MergeRatesInternal oldMap newMap tail newAcc
            | Some(_,time) ->
                let newBalance,newTime = newMap.[address]
                let newAcc =
                    if (newTime > time) then
                        accumulator.Add(address, (newBalance,newTime))
                    else
                        accumulator
                MergeRatesInternal oldMap newMap tail newAcc

    let private MergeRates (oldMap: Map<'K, CachedValue<'V>>) (newMap: Map<'K, CachedValue<'V>>) =
        let currencyList = Map.toList newMap |> List.map fst
        MergeRatesInternal oldMap newMap currencyList oldMap

    let rec private MergeBalancesInternal (oldMap: Map<Currency, Map<PublicAddress,CachedValue<'V>>>)
                                          (newMap: Map<Currency, Map<PublicAddress,CachedValue<'V>>>)
                                          (addressList: List<Currency*PublicAddress>)
                                          (accumulator: Map<Currency, Map<PublicAddress,CachedValue<'V>>>)
                                              : Map<Currency, Map<PublicAddress,CachedValue<'V>>> =
        match addressList with
        | [] -> accumulator
        | (currency,address)::tail ->
            let maybeCachedBalances = Map.tryFind currency oldMap
            match maybeCachedBalances with
            | None ->
                let newCachedBalance = newMap.[currency].[address]
                let newCachedBalancesForThisCurrency = [(address,newCachedBalance)] |> Map.ofList
                let newAcc = accumulator.Add(currency, newCachedBalancesForThisCurrency)
                MergeBalancesInternal oldMap newMap tail newAcc
            | Some(balancesMapForCurrency) ->
                let accBalancesForThisCurrency = accumulator.[currency]
                let maybeCachedBalance = Map.tryFind address balancesMapForCurrency
                match maybeCachedBalance with
                | None ->
                    let newCachedBalance = newMap.[currency].[address]
                    let newAccBalances = accBalancesForThisCurrency.Add(address, newCachedBalance)
                    let newAcc = accumulator.Add(currency, newAccBalances)
                    MergeBalancesInternal oldMap newMap tail newAcc
                | Some(_,time) ->
                    let newBalance,newTime = newMap.[currency].[address]
                    let newAcc =
                        if (newTime > time) then
                            let newAccBalances = accBalancesForThisCurrency.Add(address, (newBalance,newTime))
                            accumulator.Add(currency, newAccBalances)
                        else
                            accumulator
                    MergeBalancesInternal oldMap newMap tail newAcc

    let private MergeBalances (oldMap: Map<Currency, Map<PublicAddress,CachedValue<'V>>>)
                              (newMap: Map<Currency, Map<PublicAddress,CachedValue<'V>>>)
                                  : Map<Currency, Map<PublicAddress,CachedValue<'V>>> =
        let addressList =
            seq {
                for currency,subMap in Map.toList newMap do
                    for address,_ in Map.toList subMap do
                        yield (currency,address)
            } |> List.ofSeq
        MergeBalancesInternal oldMap newMap addressList oldMap

    // taken from http://www.fssnip.net/2z/title/All-combinations-of-list-elements
    let ListCombinations lst =
        let rec comb accLst elemLst =
            match elemLst with
            | h::t ->
                let next = [h]::List.map (fun el -> h::el) accLst @ accLst
                comb next t
            | _ -> accLst
        comb List.Empty lst

    let MapCombinations<'K,'V when 'K: comparison> (map: Map<'K,'V>): List<List<'K*'V>> =
        Map.toList map |> ListCombinations

    type MainCache(maybeCacheFiles: Option<CacheFiles>, unconfTxExpirationSpan: TimeSpan) =
        let cacheFiles =
            match maybeCacheFiles with
            | Some files -> files
            | None -> defaultCacheFiles

        let SaveNetworkDataToDisk (newCachedData: CachedNetworkData) =
            let networkDataInJson = Marshalling.Serialize newCachedData

            // it is assumed that SaveToDisk is being run under a lock() block
            File.WriteAllText (cacheFiles.CachedNetworkData.FullName, networkDataInJson)

        // we return back the rankings because the serialization process could remove dupes (and deserialization time
        // is basically negligible, i.e. took 15 milliseconds max in my MacBook in Debug mode)
        let SaveServerRankingsToDisk (serverStats: ServerRanking): ServerRanking =
            let serverStatsInJson = ServerRegistry.Serialize serverStats

            // it is assumed that SaveToDisk is being run under a lock() block
            File.WriteAllText (cacheFiles.ServerStats.FullName, serverStatsInJson)

            match LoadFromDiskInternal<ServerRanking> cacheFiles.ServerStats with
            | None -> failwith "should return something after having saved it"
            | Some cleansedServerStats -> cleansedServerStats

        let InitServers (lastServerStats: ServerRanking) =
            let mergedServers = ServerRegistry.MergeWithBaseline lastServerStats
            let mergedAndSaved = SaveServerRankingsToDisk mergedServers
            for KeyValue(currency,servers) in mergedAndSaved do
                for server in servers do
                    if server.CommunicationHistory.IsNone then
                        Infrastructure.LogError (SPrintF2 "WARNING: no history stats about %A server %s"
                                                         currency server.ServerInfo.NetworkPath)
            mergedServers

        let firstRun,initialSessionCachedNetworkData,lastServerStats = LoadFromDisk cacheFiles
        let initialServerStats = InitServers lastServerStats

        let mutable sessionCachedNetworkData = initialSessionCachedNetworkData
        let mutable sessionServerRanking = initialServerStats

        let GetSumOfAllTransactions (trans: Map<Currency,Map<PublicAddress,Map<string,CachedValue<decimal>>>>)
                                    currency address: decimal =
            let now = DateTime.UtcNow
            let currencyTrans = trans.TryFind currency
            match currencyTrans with
            | None -> 0m
            | Some someMap ->
                let addressTrans = someMap.TryFind address
                match addressTrans with
                | None -> 0m
                | Some someMap ->
                    Map.toSeq someMap |>
                        Seq.sumBy (fun (_,(txAmount,txDate)) ->
                                        // FIXME: develop some kind of cache cleanup to remove these expired txs?
                                        if (now < txDate + unconfTxExpirationSpan) then
                                            txAmount
                                        else
                                            0m
                                  )

        let rec RemoveRangeFromMap (map: Map<'K,'V>) (list: List<'K*'V>) =
            match list with
            | [] -> map
            | (key,_)::tail -> RemoveRangeFromMap (map.Remove key) tail

        let GatherDebuggingInfo (previousBalance) (currency) (address) (newCache) =
            let json1 = Marshalling.Serialize previousBalance
            let json2 = Marshalling.Serialize currency
            let json3 = Marshalling.Serialize address
            let json4 = Marshalling.Serialize newCache
            String.Join(Environment.NewLine, json1, json2, json3, json4)

        let ReportProblem (negativeBalance: decimal) (previousBalance) (currency) (address) (newCache) =
            Infrastructure.ReportError (SPrintF2 "Negative balance '%s'. Details: %s"
                                                    (negativeBalance.ToString())
                                                    (GatherDebuggingInfo
                                                        previousBalance
                                                        currency
                                                        address
                                                        newCache))

        member __.ClearAll () =
            SaveNetworkDataToDisk CachedNetworkData.Empty
            SaveServerRankingsToDisk Map.empty
            |> ignore<ServerRanking>

        member __.SaveSnapshot(newDietCachedData: DietCache) =
            let newCachedData = CachedNetworkData.FromDietCache newDietCachedData
            lock cacheFiles.CachedNetworkData (fun _ ->
                let newSessionCachedNetworkData =
                    let mergedBalances = MergeBalances sessionCachedNetworkData.Balances newCachedData.Balances
                    let mergedUsdPrices = MergeRates sessionCachedNetworkData.UsdPrice newCachedData.UsdPrice
                    {
                        sessionCachedNetworkData with
                            UsdPrice = mergedUsdPrices
                            Balances = mergedBalances
                    }

                sessionCachedNetworkData <- newSessionCachedNetworkData
                SaveNetworkDataToDisk newSessionCachedNetworkData
            )

        member __.GetLastCachedData (): CachedNetworkData =
            lock cacheFiles.CachedNetworkData (fun _ ->
                sessionCachedNetworkData
            )

        member __.RetrieveLastKnownUsdPrice currency: NotFresh<decimal> =
            lock cacheFiles.CachedNetworkData (fun _ ->
                try
                    Cached(sessionCachedNetworkData.UsdPrice.Item currency)
                with
                // FIXME: rather use tryFind func instead of using a try-with block
                | :? System.Collections.Generic.KeyNotFoundException -> NotAvailable
            )

        member __.StoreLastFiatUsdPrice (currency, lastFiatUsdPrice: decimal): unit =
            lock cacheFiles.CachedNetworkData (fun _ ->
                let time = DateTime.UtcNow

                let newCachedValue =
                    { sessionCachedNetworkData
                        with UsdPrice = sessionCachedNetworkData.UsdPrice.Add(currency, (lastFiatUsdPrice, time)) }
                sessionCachedNetworkData <- newCachedValue

                SaveNetworkDataToDisk newCachedValue
            )

        member __.RetrieveLastCompoundBalance (address: PublicAddress) (currency: Currency): NotFresh<decimal> =
            lock cacheFiles.CachedNetworkData (fun _ ->
                let balance =
                    try
                        Cached((sessionCachedNetworkData.Balances.Item currency).Item address)
                    with
                    // FIXME: rather use tryFind func instead of using a try-with block
                    | :? System.Collections.Generic.KeyNotFoundException -> NotAvailable
                match balance with
                | NotAvailable ->
                    NotAvailable
                | Cached(balance,time) ->
                    let allTransSum =
                        GetSumOfAllTransactions sessionCachedNetworkData.OutgoingTransactions currency address
                    let compoundBalance = balance - allTransSum
                    if (compoundBalance < 0.0m) then
                        ReportProblem compoundBalance
                                      None
                                      currency
                                      address
                                      sessionCachedNetworkData
                        |> ignore<bool>
                        // FIXME: should we return here just balance, or NotAvailable (as in there's no cache), and remove all transactions?
                        Cached(0.0m,time)
                    else
                        Cached(compoundBalance,time)
            )

        member self.TryRetrieveLastCompoundBalance (address: PublicAddress) (currency: Currency): Option<decimal> =
            let maybeCachedBalance = self.RetrieveLastCompoundBalance address currency
            match maybeCachedBalance with
            | NotAvailable ->
                None
            | Cached(cachedBalance,_) ->
                Some cachedBalance

        member __.RetrieveAndUpdateLastCompoundBalance (address: PublicAddress)
                                                       (currency: Currency)
                                                       (newBalance: decimal)
                                                           : CachedValue<decimal> =
            let time = DateTime.UtcNow
            lock cacheFiles.CachedNetworkData (fun _ ->
                let newCachedValueWithNewBalance,previousBalance =
                    let newCurrencyBalances,previousBalance =
                        match sessionCachedNetworkData.Balances.TryFind currency with
                        | None ->
                            Map.empty,None
                        | Some currencyBalances ->
                            let maybePreviousBalance = currencyBalances.TryFind address
                            currencyBalances,maybePreviousBalance
                    {
                        sessionCachedNetworkData with
                            Balances = sessionCachedNetworkData.Balances.Add(currency,
                                                                             newCurrencyBalances.Add(address,
                                                                                                     (newBalance, time)))
                    },previousBalance

                let newCachedValueWithNewBalanceAndMaybeLessTransactions =
                    let maybeNewValue =
                        FSharpUtil.option {
                            let! previousCachedBalance,_ = previousBalance
                            do!
                                if newBalance <> previousCachedBalance && previousCachedBalance > newBalance then
                                    Some ()
                                else
                                    None
                            let! currencyAddresses = newCachedValueWithNewBalance.OutgoingTransactions.TryFind currency
                            let! addressTransactions = currencyAddresses.TryFind address
                            let allCombinationsOfTransactions = MapCombinations addressTransactions
                            let newAddressTransactions =
                                match List.tryFind (fun combination ->
                                                       let txSumAmount = List.sumBy (fun (_,(txAmount,_)) ->
                                                                                         txAmount) combination
                                                       previousCachedBalance - txSumAmount = newBalance
                                                   ) allCombinationsOfTransactions with
                                | None ->
                                    addressTransactions
                                | Some combination ->
                                    RemoveRangeFromMap addressTransactions combination
                            let newOutgoingTransactions =
                                newCachedValueWithNewBalance
                                    .OutgoingTransactions.Add(currency,
                                                              currencyAddresses.Add(address,
                                                                                    newAddressTransactions))
                            return
                                {
                                    newCachedValueWithNewBalance with
                                        OutgoingTransactions = newOutgoingTransactions
                                }
                        }
                    match maybeNewValue with
                    | None -> newCachedValueWithNewBalance
                    | Some x -> x

                sessionCachedNetworkData <- newCachedValueWithNewBalanceAndMaybeLessTransactions

                SaveNetworkDataToDisk newCachedValueWithNewBalanceAndMaybeLessTransactions

                let allTransSum =
                    GetSumOfAllTransactions newCachedValueWithNewBalanceAndMaybeLessTransactions.OutgoingTransactions
                                            currency
                                            address
                let compoundBalance = newBalance - allTransSum
                if (compoundBalance < 0.0m) then
                    ReportProblem compoundBalance
                                  previousBalance
                                  currency
                                  address
                                  newCachedValueWithNewBalanceAndMaybeLessTransactions
                    |> ignore<bool>
                    // FIXME: should we return here just newBalance, and remove all transactions?
                    0.0m,time
                else
                    compoundBalance,time
            )

        member private __.StoreTransactionRecord (address: PublicAddress)
                                                 (currency: Currency)
                                                 (txId: string)
                                                 (amount: decimal)
                                                     : unit =
            let time = DateTime.UtcNow
            lock cacheFiles.CachedNetworkData (fun _ ->
                let newCurrencyAddresses =
                    match sessionCachedNetworkData.OutgoingTransactions.TryFind currency with
                    | None ->
                        Map.empty
                    | Some currencyAddresses ->
                        currencyAddresses
                let newAddressTransactions =
                    match newCurrencyAddresses.TryFind address with
                    | None ->
                        Map.empty.Add(txId, (amount, time))
                    | Some addressTransactions ->
                        addressTransactions.Add(txId, (amount, time))

                let newOutgoingTxs =
                    sessionCachedNetworkData.OutgoingTransactions.Add(currency,
                                                                      newCurrencyAddresses.Add(address,
                                                                                               newAddressTransactions))
                let newCachedValue = { sessionCachedNetworkData with OutgoingTransactions = newOutgoingTxs }

                sessionCachedNetworkData <- newCachedValue

                SaveNetworkDataToDisk newCachedValue
            )

        member self.StoreOutgoingTransaction (address: PublicAddress)
                                             (transactionCurrency: Currency)
                                             (feeCurrency: Currency)
                                             (txId: string)
                                             (amount: decimal)
                                             (feeAmount: decimal)
                                                 : unit =

            self.StoreTransactionRecord address transactionCurrency txId amount
            if transactionCurrency <> feeCurrency && (not Config.EthTokenEstimationCouldBeBuggyAsInNotAccurate) then
                self.StoreTransactionRecord address feeCurrency txId feeAmount

        member __.SaveServerLastStat (serverMatchFunc: ServerDetails->bool)
                                     (stat: HistoryFact): unit =
            lock cacheFiles.ServerStats (fun _ ->
                let currency,serverInfo,previousLastSuccessfulCommunication =
                    match ServerRegistry.TryFindValue sessionServerRanking serverMatchFunc with
                    | None ->
                        failwith "Merge&Save didn't happen before launching the FaultTolerantPClient?"
                    | Some (currency,server) ->
                            match server.CommunicationHistory with
                            | None -> currency,server.ServerInfo,None
                            | Some (prevHistoryInfo,lastComm) ->
                                    match prevHistoryInfo.Status with
                                    | Success -> currency,server.ServerInfo,Some lastComm
                                    | Fault faultInfo -> currency,server.ServerInfo,faultInfo.LastSuccessfulCommunication

                let now = DateTime.Now
                let newHistoryInfo: CachedValue<HistoryInfo> =
                    match stat.Fault with
                    | None ->
                        ({ TimeSpan = stat.TimeSpan; Status = Success }, now)
                    | Some exInfo ->
                        ({ TimeSpan = stat.TimeSpan
                           Status = Fault { Exception = exInfo
                                            LastSuccessfulCommunication = previousLastSuccessfulCommunication }}, now)

                let newServerDetails =
                    {
                        ServerInfo = serverInfo
                        CommunicationHistory = Some newHistoryInfo
                    }
                let serversForCurrency =
                    match sessionServerRanking.TryFind currency with
                    | None -> Seq.empty
                    | Some servers -> servers

                let newServersForCurrency =
                    Seq.append (seq { yield newServerDetails }) serversForCurrency

                let newServerList = sessionServerRanking.Add(currency, newServersForCurrency)

                let newCachedValue = SaveServerRankingsToDisk newServerList
                sessionServerRanking <- newCachedValue
            )

        member __.GetServers (currency: Currency): seq<ServerDetails> =
            lock cacheFiles.ServerStats (fun _ ->
                match sessionServerRanking.TryFind currency with
                | None ->
                    failwith <| SPrintF1 "Initialization of servers' cache failed? currency %A not found" currency
                | Some servers -> servers
            )

        member __.ExportServers (): Option<string> =
            lock cacheFiles.ServerStats (fun _ ->
                LoadFromDiskInner cacheFiles.ServerStats
            )

        member private __.BootstrapServerStatsFromTrustedSourceInternal(): Async<unit> =
            let downloadFile url: Async<Option<string>> =
                let tryDownloadFile url: Async<string> =
                    async {
                        use httpClient = new HttpClient()
                        let uri = Uri url
                        let! response = Async.AwaitTask (httpClient.GetAsync uri)
                        let isSuccess = response.IsSuccessStatusCode
                        let! content = Async.AwaitTask <| response.Content.ReadAsStringAsync()
                        if isSuccess then
                            return content
                        else
                            Infrastructure.LogError ("WARNING: error trying to retrieve server stats: " + content)
                            return failwith content
                    }
                async {
                    try
                        let! content = tryDownloadFile url
                        return Some content
                    with
                    // should we specify HttpRequestException?
                    | ex ->
                        Infrastructure.ReportWarning ex
                        |> ignore<bool>
                        return None
                }

            let targetBranch = "frontend"
            let orgName1 = "nblockchain"
            let orgName2 = "World"
            let projName = "geewallet"
            let ghBaseUrl,glBaseUrl,gnomeBaseUrl =
                "https://raw.githubusercontent.com","https://gitlab.com","https://gitlab.gnome.org"
            let pathToFile = SPrintF1 "src/GWallet.Backend/%s" ServerRegistry.ServersEmbeddedResourceFileName

            let gitHub =
                SPrintF5 "%s/%s/%s/%s/%s"
                        ghBaseUrl orgName1 projName targetBranch pathToFile

            let gitLab =
                SPrintF5 "%s/%s/%s/raw/%s/%s"
                        glBaseUrl orgName1 projName targetBranch pathToFile

            // not using GNOME hosting anymore
            let _gnomeGitLab =
                SPrintF5 "%s/%s/%s/raw/%s/%s"
                        gnomeBaseUrl orgName2 projName targetBranch pathToFile

            let allUrls = [ gitHub; gitLab ]
            let allJobs =
                allUrls |> Seq.map downloadFile

            async {
                let! maybeLastServerStatsInJson = Async.Choice allJobs
                match maybeLastServerStatsInJson with
                | None ->
                    Infrastructure.LogError "WARNING: Couldn't reach a trusted server to retrieve server stats to bootstrap cache, running in offline mode?"
                | Some lastServerStatsInJson ->
                    let lastServerStats = ImportFromJson<ServerRanking> lastServerStatsInJson
                    lock cacheFiles.ServerStats (fun _ ->
                        let savedServerStats = SaveServerRankingsToDisk lastServerStats
                        sessionServerRanking <- savedServerStats
                    )
            }

        member self.BootstrapServerStatsFromTrustedSource(): Async<unit> =
            async {
                if Config.BitcoinNet() = NBitcoin.Network.RegTest then
                    return ()
                else
                    return! self.BootstrapServerStatsFromTrustedSourceInternal()
            }

        member __.FirstRun
            with get() = firstRun

    let Instance = MainCache (None, TimeSpan.FromDays 1.0)
