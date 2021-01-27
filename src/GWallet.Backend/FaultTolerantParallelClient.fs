namespace GWallet.Backend

open System
open System.Linq
open System.Diagnostics
open System.Threading
open System.Threading.Tasks

open GWallet.Backend.FSharpUtil.UwpHacks

type ResourceUnavailabilityException (message: string, innerOrLastException: Exception) =
    inherit Exception (message, innerOrLastException)

type private TaskUnavailabilityException (message: string, innerException: Exception) =
    inherit ResourceUnavailabilityException (message, innerException)

type private ServerUnavailabilityException (message: string, lastException: Exception) =
    inherit ResourceUnavailabilityException (message, lastException)

type private NoneAvailableException (message:string, lastException: Exception) =
    inherit ServerUnavailabilityException (message, lastException)

type private NotEnoughAvailableException (message:string, lastException: Exception) =
    inherit ServerUnavailabilityException (message, lastException)

type ResultInconsistencyException (totalNumberOfSuccesfulResultsObtained: int,
                                   maxNumberOfConsistentResultsObtained: int,
                                   numberOfConsistentResultsRequired: uint32) =
  inherit Exception ("Results obtained were not enough to be considered consistent" +
                      SPrintF3 " (received: %i, consistent: %i, required: %i)"
                                  totalNumberOfSuccesfulResultsObtained
                                  maxNumberOfConsistentResultsObtained
                                  numberOfConsistentResultsRequired)

type UnsuccessfulServer<'K,'R when 'K: equality and 'K :> ICommunicationHistory> =
    {
        Server: Server<'K,'R>
        Failure: Exception
    }
type ExecutedServers<'K,'R when 'K: equality and 'K :> ICommunicationHistory> =
    {
        SuccessfulResults: List<'R>
        UnsuccessfulServers: List<UnsuccessfulServer<'K,'R>>
    }
type internal FinalResult<'K,'T,'R when 'K: equality and 'K :> ICommunicationHistory> =
    | ConsistentResult of 'R
    | AverageResult of 'R
    | InconsistentOrNotEnoughResults of ExecutedServers<'K,'R>

type ServerResult<'K,'R when 'K: equality and 'K :> ICommunicationHistory> =
    | SuccessfulResult of 'R
    | Failure of UnsuccessfulServer<'K,'R>

type ConsistencySettings<'R> =

    // fun passed represents if cached value matches or not
    | OneServerConsistentWithCertainValueOrTwoServers of ('R->bool)

    | SpecificNumberOfConsistentResponsesRequired of uint32
    | AverageBetweenResponses of (uint32 * (List<'R> -> 'R))

type ServerSelectionMode =
    | Fast
    | Analysis

type ResultSelectionSettings<'R> =
    {
        ServerSelectionMode: ServerSelectionMode
        ReportUncanceledJobs: bool
        ConsistencyConfig: ConsistencySettings<'R>
    }

type ResultSelectionMode<'R> =
    | Selective of ResultSelectionSettings<'R>
    | Exhaustive

type FaultTolerantParallelClientSettings<'R> =
    {
        NumberOfParallelJobsAllowed: uint32;
        NumberOfRetries: uint32;
        NumberOfRetriesForInconsistency: uint32;
        ResultSelectionMode: ResultSelectionMode<'R>
        ExceptionHandler: Option<Exception->unit>
    }

type MutableStateUnsafeAccessor<'T>(initialState: 'T) =
    let mutable state = initialState
    member __.Value
        with get() =
            state
         and set value =
            state <- value

type MutableStateCapsule<'T>(initialState: 'T) =
    let state = MutableStateUnsafeAccessor initialState
    let lockObject = Object()
    member __.SafeDo (func: MutableStateUnsafeAccessor<'T>->'R): 'R =
        lock lockObject (fun _ -> func state)

type ServerJob<'K,'R when 'K: equality and 'K :> ICommunicationHistory> =
    {
        Job: Async<ServerResult<'K,'R>>
        Server: Server<'K,'R>
    }

type ServerTask<'K,'R when 'K: equality and 'K :> ICommunicationHistory> =
    {
        Task: Task<ServerResult<'K,'R>>
        Server: Server<'K,'R>
        CancellationTokenSource: CancellationTokenSource
    }
    with
        static member WhenAny(tasks: seq<ServerTask<'K,'R>>) =
            async {
                let task = Task.WhenAny(tasks.Select(fun t -> t.Task))
                let! fastestTask = Async.AwaitTask task
                let correspondingTask = tasks.Single(fun t -> t.Task = fastestTask)
                return correspondingTask
            }

type internal ClientCancelStateInner =
    | Canceled of DateTime
    | Alive of List<CancellationTokenSource>
type internal ClientCancelState = MutableStateCapsule<ClientCancelStateInner>


type internal Runner<'Resource when 'Resource: equality> =
    static member Run<'K,'Ex when 'K: equality and 'K :> ICommunicationHistory and 'Ex :> Exception>
                      (server: Server<'K,'Resource>)
                      (stopwatch: Stopwatch)
                      (cancelState: ClientCancelState)
                      (shouldReportUncanceledJobs: bool)
                      (maybeExceptionHandler: Option<Exception->unit>)
                          : Async<Either<'Resource,Exception>> =
        async {
            try
                try
                    let! res = server.Retrieval
                    return SuccessfulValue res
                finally
                    stopwatch.Stop()
            with
            | ex ->

                // because if an exception happens roughly at the same time as cancellation, we don't care so much
                let isLateEnoughToReportProblem (state: ClientCancelStateInner) =
                    match state with
                    | Alive _ -> false
                    | Canceled date ->
                        (date + TimeSpan.FromSeconds 1.) < DateTime.UtcNow

                let report = Config.DebugLog &&
                             shouldReportUncanceledJobs &&
                             cancelState.SafeDo(fun state -> isLateEnoughToReportProblem state.Value)

                let maybeSpecificEx = FSharpUtil.FindException<'Ex> ex
                match maybeSpecificEx with
                | Some specificInnerEx ->
                    if report then
                        Infrastructure.LogError (SPrintF1 "Cancellation fault warning: %s"
                                                     (ex.ToString()))
                    return FailureResult (specificInnerEx :> Exception)
                | None ->
                    match maybeExceptionHandler with
                    | None -> return raise <| FSharpUtil.ReRaise ex
                    | Some exceptionHandler ->
                        exceptionHandler ex
                        return FailureResult ex
        }

    static member CreateAsyncJobFromFunc<'K,'Ex when 'K: equality and 'K :> ICommunicationHistory and 'Ex :> Exception>
                                         (shouldReportUncanceledJobs: bool)
                                         (exceptionHandler: Option<Exception->unit>)
                                         (cancelState: ClientCancelState)
                                         (updateServer: ('K->bool)->HistoryFact->unit)
                                         (server: Server<'K,'Resource>)
                                             : ServerJob<'K,'Resource> =
        let job = async {
            let stopwatch = Stopwatch()
            stopwatch.Start()

            let! runResult =
                Runner.Run<'K,'Ex> server stopwatch cancelState shouldReportUncanceledJobs exceptionHandler

            match runResult with
            | SuccessfulValue result ->
                let historyFact = { TimeSpan = stopwatch.Elapsed; Fault = None }
                updateServer (fun srv -> srv = server.Details) historyFact
                return SuccessfulResult result
            | FailureResult ex ->
                let exInfo =
                    {
                        TypeFullName = ex.GetType().FullName
                        Message = ex.Message
                    }
                let historyFact = { TimeSpan = stopwatch.Elapsed; Fault = (Some exInfo) }
                updateServer (fun srv -> srv = server.Details) historyFact
                return
                    Failure
                        {
                            Server = server
                            Failure = ex
                        }
        }

        { Job = job; Server = server }

    static member CreateJobs<'K,'Ex when 'K: equality and 'K :> ICommunicationHistory and 'Ex :> Exception>
                             (shouldReportUncanceledJobs: bool)
                             (parallelJobs: uint32)
                             (exceptionHandler: Option<Exception->unit>)
                             (updateServerFunc: ('K->bool)->HistoryFact->unit)
                             (funcs: List<Server<'K,'Resource>>)
                             (cancelState: ClientCancelState)
                                 : List<ServerJob<'K,'Resource>>*List<ServerJob<'K,'Resource>> =
        let launchFunc = Runner.CreateAsyncJobFromFunc<'K,'Ex> shouldReportUncanceledJobs
                                                               exceptionHandler
                                                               cancelState
                                                               updateServerFunc
        let jobs = funcs
                   |> Seq.map launchFunc
                   |> List.ofSeq
        if parallelJobs < uint32 jobs.Length then
            List.splitAt (int parallelJobs) jobs
        else
            jobs,List.empty


exception AlreadyCanceled

type CustomCancelSource() =

    let canceled = Event<unit>()
    let mutable canceledAlready = false
    let lockObj = Object()

    member __.Cancel() =
        lock lockObj (fun _ ->
            if canceledAlready then
                raise <| ObjectDisposedException "Already canceled/disposed"
            canceledAlready <- true
        )
        canceled.Trigger()

    [<CLIEvent>]
    member __.Canceled
        with get() =
            lock lockObj (fun _ ->
                if canceledAlready then
                    raise <| AlreadyCanceled
                canceled.Publish
            )

    interface IDisposable with
        member self.Dispose() =
            try
                self.Cancel()
            with
            | :? ObjectDisposedException ->
                ()
            // TODO: cleanup also subscribed handlers? see https://stackoverflow.com/q/58912910/544947


type FaultTolerantParallelClient<'K,'E when 'K: equality and 'K :> ICommunicationHistory and 'E :> Exception>
        (updateServer: ('K->bool)->HistoryFact->unit) =
    do
        if typeof<'E> = typeof<Exception> then
            raise <| ArgumentException ("'E cannot be System.Exception, use a derived one", "'E")

    let MeasureConsistency (results: List<'R>) =
        results |> Seq.countBy id |> Seq.sortByDescending (fun (_,count: int) -> count) |> List.ofSeq

    let LaunchAsyncJob (job: ServerJob<'K,'R>)
                           : ServerTask<'K,'R> =
        let cancellationSource = new CancellationTokenSource ()
        let token =
            try
                cancellationSource.Token
            with
            | :? ObjectDisposedException as ex ->
                raise <| TaskUnavailabilityException("cancellationTokenSource already disposed", ex)
        let task = Async.StartAsTask(job.Job, ?cancellationToken = Some token)

        let serverTask = {
            Task = task
            Server = job.Server
            CancellationTokenSource = cancellationSource
        }

        serverTask

    let rec WhenSomeInternal (consistencySettings: Option<ConsistencySettings<'R>>)
                             (initialServerCount: uint32)
                             (startedTasks: List<ServerTask<'K,'R>>)
                             (jobsToLaunchLater: List<ServerJob<'K,'R>>)
                             (resultsSoFar: List<'R>)
                             (failedFuncsSoFar: List<UnsuccessfulServer<'K,'R>>)
                             (cancellationSource: Option<CustomCancelSource>)
                             (cancelState: ClientCancelState)
                                 : Async<FinalResult<'K,'T,'R>> = async {
        if startedTasks = List.Empty then
            return
                InconsistentOrNotEnoughResults
                    {
                        SuccessfulResults = resultsSoFar
                        UnsuccessfulServers = failedFuncsSoFar
                    }
        else
            let jobToWaitForFirstFinishedTask = ServerTask.WhenAny startedTasks
            let! fastestTask = jobToWaitForFirstFinishedTask

            let restOfTasks =
                startedTasks.Where(fun task -> not (task = fastestTask)) |> List.ofSeq

            let newResults,newFailedFuncs =
                match fastestTask.Task.Result with
                | Failure unsuccessfulServer ->
                    resultsSoFar,unsuccessfulServer::failedFuncsSoFar
                | SuccessfulResult newResult ->
                    newResult::resultsSoFar,failedFuncsSoFar

            fastestTask.CancellationTokenSource.Dispose()

            let newRestOfTasks,newRestOfJobs =
                match jobsToLaunchLater with
                | [] ->
                    restOfTasks,List.Empty
                | head::tail ->
                    let maybeNewTask = cancelState.SafeDo(fun state ->
                        let resultingTask =
                            match state.Value with
                            | Alive cancelSources ->
                                let newTask = LaunchAsyncJob head
                                state.Value <- Alive (newTask.CancellationTokenSource::cancelSources)
                                Some newTask
                            | Canceled _ ->
                                None
                        resultingTask
                    )
                    match maybeNewTask with
                    | Some newTask ->
                        newTask::restOfTasks,tail
                    | None ->
                        restOfTasks,tail

            let returnWithConsistencyOf (minNumberOfConsistentResultsRequired: Option<uint32>) cacheMatchFunc = async {
                let resultsSortedByCount = MeasureConsistency newResults
                match resultsSortedByCount with
                | [] ->
                    return! WhenSomeInternal consistencySettings
                                             initialServerCount
                                             newRestOfTasks
                                             newRestOfJobs
                                             newResults
                                             newFailedFuncs
                                             cancellationSource
                                             cancelState
                | (mostConsistentResult,maxNumberOfConsistentResultsObtained)::_ ->
                    match minNumberOfConsistentResultsRequired,cacheMatchFunc with
                    | None, None ->
                        return ConsistentResult mostConsistentResult
                    | Some number, Some cacheMatch ->
                        if cacheMatch mostConsistentResult || (maxNumberOfConsistentResultsObtained = int number) then
                            return ConsistentResult mostConsistentResult
                        else
                            return! WhenSomeInternal consistencySettings
                                                     initialServerCount
                                                     newRestOfTasks
                                                     newRestOfJobs
                                                     newResults
                                                     newFailedFuncs
                                                     cancellationSource
                                                     cancelState
                    | _ -> return failwith "should be either both None or both Some!"
            }

            match consistencySettings with
            | Some (AverageBetweenResponses (minimumNumberOfResponses,averageFunc)) ->
                if newResults.Length >= int minimumNumberOfResponses then
                    return AverageResult (averageFunc newResults)
                else
                    return! WhenSomeInternal consistencySettings
                                             initialServerCount
                                             newRestOfTasks
                                             newRestOfJobs
                                             newResults
                                             newFailedFuncs
                                             cancellationSource
                                             cancelState
            | Some (SpecificNumberOfConsistentResponsesRequired number) ->
                return! returnWithConsistencyOf (Some number) ((fun _ -> false) |> Some)
            | Some (OneServerConsistentWithCertainValueOrTwoServers cacheMatchFunc) ->
                return! returnWithConsistencyOf (Some 2u) (Some cacheMatchFunc)
            | None ->
                if newRestOfTasks.Length = 0 then

                    Infrastructure.LogDebug "100% done (for this currency)"
                    return! returnWithConsistencyOf None None

                else
                    Infrastructure.LogDebug (SPrintF1 "%f%% done (for this currency)"
                            (100.*(float (newFailedFuncs.Length+newResults.Length))/(float initialServerCount)))

                    return! WhenSomeInternal consistencySettings
                                             initialServerCount
                                             newRestOfTasks
                                             newRestOfJobs
                                             newResults
                                             newFailedFuncs
                                             cancellationSource
                                             cancelState
    }

    let CancelAndDispose (cancelState: ClientCancelState) =
        cancelState.SafeDo(
            fun state ->
                match state.Value with
                | Canceled _ ->
                    ()
                | Alive cancelSources ->
                    for cancelSource in cancelSources do
                        try
                            cancelSource.Cancel ()
                            cancelSource.Dispose ()
                        with
                        | :? ObjectDisposedException ->
                            ()

                    state.Value <- Canceled DateTime.UtcNow
        )

    // at the time of writing this, I only found a Task.WhenAny() equivalent function in the asyncF# world, called
    // "Async.WhenAny" in TomasP's tryJoinads source code, however it seemed a bit complex for me to wrap my head around
    // it (and I couldn't just consume it and call it a day, I had to modify it to be "WhenSome" instead of "WhenAny",
    // as in when N>1), so I decided to write my own, using Tasks to make sure I would not spawn duplicate jobs
    let WhenSome (settings: FaultTolerantParallelClientSettings<'R>)
                 consistencyConfig
                 (funcs: List<Server<'K,'R>>)
                 (resultsSoFar: List<'R>)
                 (failedFuncsSoFar: List<UnsuccessfulServer<'K,'R>>)
                 (cancellationSource: Option<CustomCancelSource>)
                     : Async<FinalResult<'K,'T,'R>> =

        let initialServerCount = funcs.Length |> uint32

        let shouldReportUncanceledJobs =
            match settings.ResultSelectionMode with
            | Exhaustive -> false
            | Selective subSettings ->
                subSettings.ReportUncanceledJobs

        let cancelState = ClientCancelState (Alive List.empty)

        let maybeJobs = cancelState.SafeDo(fun state ->
            match state.Value with
            | Canceled _ -> None
            | Alive _ ->
                Some <| Runner<'R>.CreateJobs<'K,'E> shouldReportUncanceledJobs
                                                     settings.NumberOfParallelJobsAllowed
                                                     settings.ExceptionHandler
                                                     updateServer
                                                     funcs
                                                     cancelState
        )

        let startedTasks,jobsToLaunchLater =
            match maybeJobs with
            | None ->
                raise <| TaskCanceledException "Found canceled when about to launch more jobs"
            | Some (firstJobsToLaunch,jobsToLaunchLater) ->
                match cancellationSource with
                | None -> ()
                | Some customCancelSource ->
                    try
                        customCancelSource.Canceled.Add(fun _ ->
                            CancelAndDispose cancelState
                        )
                    with
                    | AlreadyCanceled ->
                        raise <| TaskCanceledException(
                                     "Found canceled when about to subscribe to cancellation"
                                 )
                cancelState.SafeDo (fun state ->
                    match state.Value with
                    | Canceled _ ->
                        raise <| TaskCanceledException "Found canceled when about to launch more tasks"
                    | Alive currentList ->
                        let startedTasks = firstJobsToLaunch |> List.map (fun job -> LaunchAsyncJob job)
                        let newCancelSources = startedTasks |> List.map (fun task -> task.CancellationTokenSource)
                        state.Value <- Alive (List.append currentList newCancelSources)
                        startedTasks,jobsToLaunchLater
                )

        let job = WhenSomeInternal consistencyConfig
                         initialServerCount
                         startedTasks
                         jobsToLaunchLater
                         resultsSoFar
                         failedFuncsSoFar
                         cancellationSource
                         cancelState
        let jobWithCancellation =
            async {
                try
                    let! res = job
                    return res
                finally
                    CancelAndDispose cancelState
            }
        jobWithCancellation

    let rec QueryInternalImplementation
                          (settings: FaultTolerantParallelClientSettings<'R>)
                          (initialFuncCount: uint32)
                          (funcs: List<Server<'K,'R>>)
                          (resultsSoFar: List<'R>)
                          (failedFuncsSoFar: List<UnsuccessfulServer<'K,'R>>)
                          (retries: uint32)
                          (retriesForInconsistency: uint32)
                          (cancellationSource: Option<CustomCancelSource>)
                              : Async<'R> = async {
        if not (funcs.Any()) then
            return raise <| ArgumentException ("number of funcs must be higher than zero",
                                               "funcs")
        let howManyFuncs = uint32 funcs.Length
        let numberOfParallelJobsAllowed = int settings.NumberOfParallelJobsAllowed

        match settings.ResultSelectionMode with
        | Selective resultSelectionSettings ->
            match resultSelectionSettings.ConsistencyConfig with
            | SpecificNumberOfConsistentResponsesRequired numberOfConsistentResponsesRequired ->
                if numberOfConsistentResponsesRequired < 1u then
                    return raise <| ArgumentException("must be higher than zero", "numberOfConsistentResponsesRequired")
                if howManyFuncs < numberOfConsistentResponsesRequired then
                    return raise <| ArgumentException ("number of funcs must be equal or higher than numberOfConsistentResponsesRequired",
                                                       "funcs")
            | AverageBetweenResponses(minimumNumberOfResponses,_) ->
                if int minimumNumberOfResponses > numberOfParallelJobsAllowed then
                    return raise <| ArgumentException ("numberOfParallelJobsAllowed should be equal or higher than minimumNumberOfResponses for the averageFunc",
                                                       "settings")
            | OneServerConsistentWithCertainValueOrTwoServers _ ->
                ()
        | _ -> ()

        let consistencyConfig =
            match settings.ResultSelectionMode with
            | Exhaustive -> None
            | Selective subSettings -> Some subSettings.ConsistencyConfig
        let job = WhenSome settings
                           consistencyConfig
                           funcs
                           resultsSoFar
                           failedFuncsSoFar
                           cancellationSource
        let! result = job
        match result with
        | AverageResult averageResult ->
            return averageResult
        | ConsistentResult consistentResult ->
            return consistentResult
        | InconsistentOrNotEnoughResults executedServers ->
            let failedFuncs = executedServers.UnsuccessfulServers
                                  |> List.map (fun unsuccessfulServer -> unsuccessfulServer.Server)
            if executedServers.SuccessfulResults.Length = 0 then
                if retries = settings.NumberOfRetries then
                    let firstEx = executedServers.UnsuccessfulServers.First().Failure
                    return raise <| NoneAvailableException ("Not available", firstEx)
                else
                    return! QueryInternalImplementation
                                          settings
                                          initialFuncCount
                                          failedFuncs
                                          executedServers.SuccessfulResults
                                          List.Empty
                                          (retries + 1u)
                                          retriesForInconsistency
                                          cancellationSource
            else
                let totalNumberOfSuccesfulResultsObtained = executedServers.SuccessfulResults.Length

                // HACK: we do this as a quick fix wrt new OneServerConsistentWithCertainValueOrTwoServers setting, but we should
                // (TODO) rather throw a specific overload of ResultInconsistencyException about this mode being used
                let wrappedSettings =
                    match consistencyConfig with
                    | Some (OneServerConsistentWithCertainValueOrTwoServers _) ->
                        Some (SpecificNumberOfConsistentResponsesRequired 2u)
                    | _ -> consistencyConfig

                match wrappedSettings with
                | Some (SpecificNumberOfConsistentResponsesRequired numberOfConsistentResponsesRequired) ->
                    let resultsOrderedByCount = MeasureConsistency executedServers.SuccessfulResults
                    match resultsOrderedByCount with
                    | [] ->
                        return failwith "resultsSoFar.Length != 0 but MeasureConsistency returns None, please report this bug"
                    | (_,maxNumberOfConsistentResultsObtained)::_ ->
                        if retriesForInconsistency = settings.NumberOfRetriesForInconsistency then
                            return raise <| ResultInconsistencyException (totalNumberOfSuccesfulResultsObtained,
                                                                          maxNumberOfConsistentResultsObtained,
                                                                          numberOfConsistentResponsesRequired)
                        else
                            return! QueryInternalImplementation
                                                  settings
                                                  initialFuncCount
                                                  funcs
                                                  List.Empty
                                                  List.Empty
                                                  retries
                                                  (retriesForInconsistency + 1u)
                                                  cancellationSource
                | Some(AverageBetweenResponses _) ->
                    if retries = settings.NumberOfRetries then
                        let firstEx = executedServers.UnsuccessfulServers.First().Failure
                        return raise <| NotEnoughAvailableException ("resultsSoFar.Length != 0 but not enough to satisfy minimum number of results for averaging func",
                                                                     firstEx)
                    else
                        return! QueryInternalImplementation
                                              settings
                                              initialFuncCount
                                              failedFuncs
                                              executedServers.SuccessfulResults
                                              executedServers.UnsuccessfulServers
                                              (retries + 1u)
                                              retriesForInconsistency
                                              cancellationSource
                | _ ->
                    return failwith "wrapping settings didn't work?"

    }

    let SortServers (servers: List<Server<'K,'R>>) (mode: ServerSelectionMode): List<Server<'K,'R>> =
        let workingServers = List.filter (fun server ->
                                             match server.Details.CommunicationHistory with
                                             | None ->
                                                 false
                                             | Some historyInfo ->
                                                 match historyInfo.Status with
                                                 | Fault _ ->
                                                     false
                                                 | _ ->
                                                     true
                                         ) servers
        let sortedWorkingServers =
            List.sortBy
                (fun server ->
                    match server.Details.CommunicationHistory with
                    | None ->
                        failwith "previous filter didn't work? should get working servers only, not lacking history"
                    | Some historyInfo ->
                        match historyInfo.Status with
                        | Fault _ ->
                            failwith "previous filter didn't work? should get working servers only, not faulty"
                        | _ ->
                            historyInfo.TimeSpan
                )
                workingServers

        let serversWithNoHistoryServers = List.filter (fun server -> server.Details.CommunicationHistory.IsNone) servers

        let faultyServers = List.filter (fun server ->
                                            match server.Details.CommunicationHistory with
                                            | None ->
                                                false
                                            | Some historyInfo ->
                                                match historyInfo.Status with
                                                | Fault _ ->
                                                    true
                                                | _ ->
                                                    false
                                        ) servers
        let sortedFaultyServers =
            List.sortBy
                (fun server ->
                    match server.Details.CommunicationHistory with
                    | None ->
                        failwith "previous filter didn't work? should get working servers only, not lacking history"
                    | Some historyInfo ->
                        match historyInfo.Status with
                        | Fault _ ->
                            historyInfo.TimeSpan
                        | _ ->
                            failwith "previous filter didn't work? should get faulty servers only, not working ones"
                )
                faultyServers

        if mode = ServerSelectionMode.Fast then
            List.append sortedWorkingServers (List.append serversWithNoHistoryServers sortedFaultyServers)
        else
            let intersectionOffset = 3u
            let result = FSharpUtil.ListIntersect
                                     (List.append serversWithNoHistoryServers sortedWorkingServers)
                                     sortedFaultyServers
                                     intersectionOffset
            let randomizationOffset = intersectionOffset + 1u
            Shuffler.RandomizeEveryNthElement result randomizationOffset

    member private __.QueryInternal<'R when 'R : equality>
                            (settings: FaultTolerantParallelClientSettings<'R>)
                            (servers: List<Server<'K,'R>>)
                            (cancellationTokenSourceOption: Option<CustomCancelSource>)
                                : Async<'R> =
        if settings.NumberOfParallelJobsAllowed < 1u then
            raise <| ArgumentException ("must be higher than zero", "numberOfParallelJobsAllowed")

        let initialServerCount = uint32 servers.Length
        let maybeSortedServers =
            match settings.ResultSelectionMode with
            | Exhaustive -> servers
            | Selective selSettings ->
                SortServers servers selSettings.ServerSelectionMode

        let job = QueryInternalImplementation
                      settings
                      initialServerCount
                      maybeSortedServers
                      List.Empty
                      List.Empty
                      0u
                      0u
                      cancellationTokenSourceOption
        async {
            let! res = job
            return res
        }

    member self.QueryWithCancellation<'R when 'R : equality>
                    (cancellationTokenSource: CustomCancelSource)
                    (settings: FaultTolerantParallelClientSettings<'R>)
                    (servers: List<Server<'K,'R>>)
                        : Async<'R> =
        self.QueryInternal<'R> settings servers (Some cancellationTokenSource)

    member self.Query<'R when 'R : equality> (settings: FaultTolerantParallelClientSettings<'R>)
                                             (servers: List<Server<'K,'R>>)
                                                 : Async<'R> =
        self.QueryInternal<'R> settings servers None
