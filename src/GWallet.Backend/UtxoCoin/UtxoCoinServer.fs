namespace GWallet.Backend.UtxoCoin

// NOTE: we can rename this file to less redundant "Server.fs" when this F# compiler bug is fixed:
// https://github.com/Microsoft/visualfsharp/issues/3231

open System

open GWallet.Backend
open GWallet.Backend.FSharpUtil.UwpHacks

type QuerySettings<'R> =
    | Default of mode: ServerSelectionMode
    | Balance of ServerSelectionMode*('R->bool)
    | FeeEstimation of (List<'R>->'R)
    | Broadcast

module Server =

    let private NumberOfParallelJobsForMode mode =
        match mode with
        | ServerSelectionMode.Fast -> 3u
        | ServerSelectionMode.Analysis -> 2u

    let private FaultTolerantParallelClientDefaultSettings (mode: ServerSelectionMode)
                                                           maybeConsistencyConfig =
        let consistencyConfig =
            match maybeConsistencyConfig with
            | None -> SpecificNumberOfConsistentResponsesRequired 2u
            | Some specificConsistencyConfig -> specificConsistencyConfig

        {
            NumberOfParallelJobsAllowed = NumberOfParallelJobsForMode mode
            NumberOfRetries = Config.NUMBER_OF_RETRIES_TO_SAME_SERVERS
            NumberOfRetriesForInconsistency = Config.NUMBER_OF_RETRIES_TO_SAME_SERVERS
            ExceptionHandler = Some (fun ex -> Infrastructure.ReportWarning ex)
            ResultSelectionMode =
                Selective
                    {
                        ServerSelectionMode = mode
                        ConsistencyConfig = consistencyConfig
                        ReportUncanceledJobs = true
                    }
        }

    let private FaultTolerantParallelClientSettingsForBroadcast() =
        FaultTolerantParallelClientDefaultSettings ServerSelectionMode.Fast
                                                   (Some (SpecificNumberOfConsistentResponsesRequired 1u))

    let private FaultTolerantParallelClientSettingsForBalanceCheck (mode: ServerSelectionMode)
                                                                   cacheOrInitialBalanceMatchFunc =
        let consistencyConfig =
            if mode = ServerSelectionMode.Fast then
                Some (OneServerConsistentWithCertainValueOrTwoServers cacheOrInitialBalanceMatchFunc)
            else
                None
        FaultTolerantParallelClientDefaultSettings mode consistencyConfig

    let private faultTolerantElectrumClient =
        FaultTolerantParallelClient<ServerDetails,ServerDiscardedException> Caching.Instance.SaveServerLastStat

    // FIXME: seems there's some code duplication between this function and EtherServer.fs's GetServerFuncs function
    //        and room for simplification to not pass a new ad-hoc delegate?
    let internal GetServerFuncs<'R> (electrumClientFunc: Async<StratumClient>->Async<'R>)
                                    (electrumServers: seq<ServerDetails>)
                                        : seq<Server<ServerDetails,'R>> =

        let ElectrumServerToRetrievalFunc (server: ServerDetails)
                                          (electrumClientFunc: Async<StratumClient>->Async<'R>)
                                              : Async<'R> = async {
            try
                let stratumClient = ElectrumClient.StratumServer server
                return! electrumClientFunc stratumClient

            // NOTE: try to make this 'with' block be in sync with the one in EtherServer:GetWeb3Funcs()
            with
            | :? CommunicationUnsuccessfulException as ex ->
                let msg = SPrintF2 "%s: %s" (ex.GetType().FullName) ex.Message
                return raise <| ServerDiscardedException(msg, ex)
            | ex ->
                return raise <| Exception(SPrintF1 "Some problem when connecting to %s" server.ServerInfo.NetworkPath,
                                          ex)
        }
        let ElectrumServerToGenericServer (electrumClientFunc: Async<StratumClient>->Async<'R>)
                                          (electrumServer: ServerDetails)
                                              : Server<ServerDetails,'R> =
            {
                Details = electrumServer
                Retrieval = ElectrumServerToRetrievalFunc electrumServer electrumClientFunc
            }

        let serverFuncs =
            Seq.map (ElectrumServerToGenericServer electrumClientFunc)
                     electrumServers
        serverFuncs

    let private GetRandomizedFuncs<'R> (currency: Currency)
                                       (electrumClientFunc: Async<StratumClient>->Async<'R>)
                                              : List<Server<ServerDetails,'R>> =

        let electrumServers = ElectrumServerSeedList.Randomize currency
        GetServerFuncs electrumClientFunc electrumServers
            |> List.ofSeq

    let Query<'R when 'R: equality> currency
                                    (settings: QuerySettings<'R>)
                                    (job: Async<StratumClient>->Async<'R>)
                                    (cancelSourceOption: Option<CustomCancelSource>)
                                        : Async<'R> =
        let query =
            match cancelSourceOption with
            | None ->
                faultTolerantElectrumClient.Query
            | Some cancelSource ->
                faultTolerantElectrumClient.QueryWithCancellation cancelSource
        let querySettings =
            match settings with
            | Default mode -> FaultTolerantParallelClientDefaultSettings mode None
            | Balance (mode,predicate) -> FaultTolerantParallelClientSettingsForBalanceCheck mode predicate
            | FeeEstimation averageFee ->
                let minResponsesRequired = 3u
                FaultTolerantParallelClientDefaultSettings
                    ServerSelectionMode.Fast
                    (Some (AverageBetweenResponses (minResponsesRequired, averageFee)))
            | Broadcast -> FaultTolerantParallelClientSettingsForBroadcast()
        query
            querySettings
            (GetRandomizedFuncs currency job)
