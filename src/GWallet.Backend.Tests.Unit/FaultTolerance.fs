namespace GWallet.Backend.Tests.Unit

open System
open System.Linq

open NUnit.Framework

open GWallet.Backend

exception DummyIrrelevantToThisTestException
exception SomeSpecificException
exception SomeException
exception SomeOtherException
type SomeInnerException() =
    inherit Exception()

[<TestFixture>]
type FaultTolerance() =

    let one_consistent_result_because_this_test_doesnt_test_consistency = 1u
    let not_more_than_one_parallel_job_because_this_test_doesnt_test_parallelization = 1u
    let two_because_its_larger_than_the_length_of_the_server_list_which_has_one_elem = 2u
    let test_does_not_involve_retries = 0u
    let dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test = (fun _ _ -> ())

    // yes, the default one is the fast one because it's the one with no filters, just sorting
    let default_result_selection_mode_as_it_is_irrelevant_for_this_test maybeConsistencyConfig =
        let consistencyConfig =
            match maybeConsistencyConfig with
            | None -> SpecificNumberOfConsistentResponsesRequired
                          one_consistent_result_because_this_test_doesnt_test_consistency
            | Some specificConsistencyConfig -> specificConsistencyConfig
        Selective
            {
                ServerSelectionMode = ServerSelectionMode.Fast
                ReportUncanceledJobs = false
                ConsistencyConfig = consistencyConfig
            }


    let some_fault_with_no_last_successful_comm_because_irrelevant_for_this_test =
        Fault { Exception = { TypeFullName = typeof<Exception>.FullName; Message = "some err" }
                LastSuccessfulCommunication = None }

    let dummy_date_for_cache = DateTime.Now

    let defaultSettingsForNoConsistencyNoParallelismAndNoRetries consistencyConfig =
        {
            NumberOfParallelJobsAllowed = not_more_than_one_parallel_job_because_this_test_doesnt_test_parallelization
            NumberOfRetries = test_does_not_involve_retries
            NumberOfRetriesForInconsistency = test_does_not_involve_retries
            ResultSelectionMode = default_result_selection_mode_as_it_is_irrelevant_for_this_test consistencyConfig
            ExceptionHandler = None
        }

    let defaultFaultTolerantParallelClient =
        FaultTolerantParallelClient<ServerDetails,SomeSpecificException>
            dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test

    let dummy_connection_type = { Encrypted = false; Protocol = Http }
    let serverWithNoHistoryInfoBecauseIrrelevantToThisTest serverId job =
        {
            Details =
                {
                    ServerInfo =
                        {
                            NetworkPath = serverId
                            ConnectionType = dummy_connection_type
                        }
                    CommunicationHistory = None
                }
            Retrieval = job
        }

    [<Test>]
    member __.``can retrieve basic T for single func``() =
        let someResult = 1
        let aJob =
            async { return someResult }
        let func = serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob" aJob
        let retrievedData =
            defaultFaultTolerantParallelClient.Query
                (defaultSettingsForNoConsistencyNoParallelismAndNoRetries None) [ func ]
                |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someResult)

    [<Test>]
    member __.``can retrieve basic T for 2 funcs``() =
        let someResult = 1
        let aJob1 =
            async { return someResult }
        let aJob2 =
            async { return someResult }
        let func1,func2 = serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob1" aJob1,
                          serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob2" aJob2
        let retrievedData = defaultFaultTolerantParallelClient.Query
                                (defaultSettingsForNoConsistencyNoParallelismAndNoRetries None)
                                [ func1; func2 ]
                                |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someResult)

    [<Test>]
    member __.``throws ArgumentException if no funcs``(): unit =
        let client = defaultFaultTolerantParallelClient
        Assert.Throws<ArgumentException>(
            fun _ -> client.Query
                            (defaultSettingsForNoConsistencyNoParallelismAndNoRetries None)
                            List.Empty
                            |> Async.RunSynchronously
                            |> ignore<unit>
        ) |> ignore<ArgumentException>

    [<Test>]
    member __.``can retrieve one if 1 of the funcs throws``() =
        let someResult = 1
        let aJob1 =
            async { return raise SomeSpecificException }
        let aJob2 =
            async { return someResult }
        let func1,func2 = serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob1" aJob1,
                          serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob2" aJob2
        let retrievedData =
            defaultFaultTolerantParallelClient.Query
                (defaultSettingsForNoConsistencyNoParallelismAndNoRetries None) [ func1; func2 ]
                    |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someResult)

    [<Test>]
    member __.``ServerUnavailabilityException exception contains innerException``() =
        let aJob1 =
            async { return raise SomeException }
        let aJob2 =
            async { return raise SomeException }

        let func1,func2 = serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob1" aJob1,
                          serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob2" aJob2

        let retrievedData =
            try
                let result =
                    (FaultTolerantParallelClient<ServerDetails,SomeException>
                        dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test)
                        .Query
                            (defaultSettingsForNoConsistencyNoParallelismAndNoRetries None) [ func1; func2 ]
                                |> Async.RunSynchronously
                Some(result)
            with
            | ex ->
                Assert.That (ex :? ResourceUnavailabilityException, Is.True)
                Assert.That (ex.GetType().Name, Is.EqualTo "NoneAvailableException")
                Assert.That (ex.InnerException, Is.Not.Null)
                Assert.That (ex.InnerException, Is.TypeOf<SomeException>())
                None

        Assert.That(retrievedData, Is.EqualTo(None))

    [<Test>]
    member __.``exception type passed in is ignored``() =
        let someResult = 1
        let aJob1 =
            async { return raise SomeException }
        let aJob2 =
            async { return someResult }
        let func1,func2 = serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob1" aJob1,
                          serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob2" aJob2

        let result =
            (FaultTolerantParallelClient<ServerDetails, SomeException>
                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test)
                .Query
                    (defaultSettingsForNoConsistencyNoParallelismAndNoRetries None) [ func1; func2 ]
                        |> Async.RunSynchronously
        Assert.That(result, Is.EqualTo(someResult))

    [<Test>]
    member __.``exception type not passed in is not ignored``() =
        let someResult = 1
        let aJob1 =
            async { return raise SomeOtherException }
        let aJob2 =
            async { return someResult }
        let func1,func2 = serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob1" aJob1,
                          serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob2" aJob2

        let ex = Assert.Throws<AggregateException>(fun _ ->
            (FaultTolerantParallelClient<ServerDetails, SomeException>
                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test)
                .Query
                    (defaultSettingsForNoConsistencyNoParallelismAndNoRetries None) [ func1; func2 ]
                        |> Async.RunSynchronously
                        |> ignore<int> )

        Assert.That((FSharpUtil.FindException<SomeOtherException> ex).IsSome, Is.True)

    [<Test>]
    member __.``exception type not passed in doesn't bubble up if exception handler is specified``() =
        let someResult = 1
        let aJob1 =
            async { return raise SomeOtherException }
        let aJob2 =
            async { return someResult }
        let aJob3 =
            async { return raise SomeOtherException }
        let func1,func2,func3 = serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob1" aJob1,
                                serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob2" aJob2,
                                serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob3" aJob3

        let mutable exceptionHandlerCalled = false

        let theResult =
                (FaultTolerantParallelClient<ServerDetails, SomeException>
                    dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test)
                    .Query
                        ({ defaultSettingsForNoConsistencyNoParallelismAndNoRetries None
                            with ExceptionHandler = Some (fun _ -> exceptionHandlerCalled <- true)})
                            [ func1; func2; func3 ]
                                |> Async.RunSynchronously

        Assert.That(theResult, Is.EqualTo someResult)

        Assert.That(exceptionHandlerCalled, Is.EqualTo true)

    [<Test>]
    member __.``exception type passed in is ignored also if it's found in an innerException'``() =
        let someResult = 1
        let aJob1 =
            async { return raise <| Exception("bar", SomeInnerException()) }
        let aJob2 =
            async { return someResult }

        let func1,func2 = serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob1" aJob1,
                          serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob2" aJob2

        let result =
            (FaultTolerantParallelClient<ServerDetails, SomeInnerException>
                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test)
                .Query
                    (defaultSettingsForNoConsistencyNoParallelismAndNoRetries None) [ func1; func2 ]
                        |> Async.RunSynchronously
        Assert.That(result, Is.EqualTo(someResult))

    [<Test>]
    member __.``exception passed in must not be SystemException, otherwise it throws``() =
        Assert.Throws<ArgumentException>(fun _ ->
            (FaultTolerantParallelClient<ServerDetails, Exception>
                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test)
                |> ignore<FaultTolerantParallelClient<ServerDetails, Exception>> ) |> ignore<ArgumentException>

    [<Test>]
    member __.``makes sure data is consistent across N funcs``() =
        let numberOfConsistentResponsesToBeConsideredSafe = 2u

        let someConsistentResult = 1
        let someInconsistentResult = 2

        let anInconsistentJob =
            async { return someInconsistentResult }
        let aConsistentJobA =
            async { return someConsistentResult }
        let aConsistentJobB =
            async { return someConsistentResult }

        let funcInconsistent,funcConsistentA,funcConsistentB =
            serverWithNoHistoryInfoBecauseIrrelevantToThisTest "anInconsistentJob" anInconsistentJob,
            serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aConsistentJobA" aConsistentJobA,
            serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aConsistentJobB" aConsistentJobB

        let consistencyCfg = SpecificNumberOfConsistentResponsesRequired numberOfConsistentResponsesToBeConsideredSafe
                                 |> Some
        let settings = defaultSettingsForNoConsistencyNoParallelismAndNoRetries consistencyCfg
        let consistencyGuardClient =
            FaultTolerantParallelClient<ServerDetails, SomeSpecificException>
                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test

        let retrievedData =
            consistencyGuardClient
                .Query settings
                       [ funcInconsistent; funcConsistentA; funcConsistentB; ]
                    |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someConsistentResult)

        let retrievedData =
            consistencyGuardClient
                .Query settings
                       [ funcConsistentA; funcInconsistent; funcConsistentB; ]
                    |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someConsistentResult)

        let retrievedData =
            consistencyGuardClient
                .Query settings
                       [ funcConsistentA; funcConsistentB; funcInconsistent; ]
                           |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someConsistentResult)

    [<Test>]
    member __.``consistency precondition > 0``() =
        let consistencyCfg = SpecificNumberOfConsistentResponsesRequired 0u |> Some
        let invalidSettings = defaultSettingsForNoConsistencyNoParallelismAndNoRetries consistencyCfg
        let dummyServers =
            [ serverWithNoHistoryInfoBecauseIrrelevantToThisTest "dummyServerName" (async { return () }) ]
        Assert.Throws<ArgumentException>(fun _ ->
            (FaultTolerantParallelClient<ServerDetails, SomeSpecificException>
                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test)
                    .Query invalidSettings dummyServers
                        |> Async.RunSynchronously
                        |> ignore<unit> ) |> ignore<ArgumentException>

    [<Test>]
    member __.``consistency precondition > funcs``() =
        let numberOfConsistentResponsesToBeConsideredSafe = 3u
        let consistencyConfig =
            SpecificNumberOfConsistentResponsesRequired numberOfConsistentResponsesToBeConsideredSafe |> Some
        let settings = defaultSettingsForNoConsistencyNoParallelismAndNoRetries consistencyConfig

        let someResult = 1

        let aJob1 =
            async { return someResult }
        let aJob2 =
            async { return someResult }

        let func1,func2 = serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob1" aJob1,
                          serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob2" aJob2

        let client = FaultTolerantParallelClient<ServerDetails, SomeSpecificException>
                         dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test

        Assert.Throws<ArgumentException>(fun _ ->
            client.Query
                settings
                [ func1; func2 ]
                    |> Async.RunSynchronously
                    |> ignore<int> ) |> ignore<ArgumentException>

    [<Test>]
    member __.``if consistency is not found, throws inconsistency exception``() =
        let numberOfConsistentResponsesToBeConsideredSafe = 3u
        let consistencyConfig =
            SpecificNumberOfConsistentResponsesRequired numberOfConsistentResponsesToBeConsideredSafe |> Some
        let settings = defaultSettingsForNoConsistencyNoParallelismAndNoRetries consistencyConfig

        let mostConsistentResult = 1
        let someOtherResultA = 2
        let someOtherResultB = 3

        let aJob1 =
            async { return someOtherResultA }
        let aJob2 =
            async { return mostConsistentResult }
        let aJob3 =
            async { return someOtherResultB }
        let aJob4 =
            async { return mostConsistentResult }

        let func1,func2,func3,func4 = serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob1" aJob1,
                                      serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob2" aJob2,
                                      serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob3" aJob3,
                                      serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob4" aJob4

        let client = FaultTolerantParallelClient<ServerDetails, SomeSpecificException>
                         dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test

        let inconsistencyEx = Assert.Throws<ResultInconsistencyException>(fun _ ->
                                  client.Query
                                      settings
                                      [ func1; func2; func3; func4 ]
                                          |> Async.RunSynchronously
                                          |> ignore<int> )
        Assert.That(inconsistencyEx.Message, Is.StringContaining("received: 4, consistent: 2, required: 3"))

    [<Test>]
    member __.``test new consistency setting designed to take advantage of caching (I)``() =
        let someBalance = 1.0m
        let someBalanceMatchFunc someRetrievedBalance =
            someRetrievedBalance = someBalance

        let consistencyConfig = OneServerConsistentWithCertainValueOrTwoServers someBalanceMatchFunc |> Some
        let settings = defaultSettingsForNoConsistencyNoParallelismAndNoRetries consistencyConfig

        let otherBalance = 2.0m
        let yetAnotherBalance = 3.0m

        let aJob1 =
            async { return otherBalance }
        let aJob2 =
            async { return yetAnotherBalance }
        let aJob3 =
            async { return someBalance }

        let func1,func2,func3 = serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob1" aJob1,
                                serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob2" aJob2,
                                serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob3" aJob3

        let client = FaultTolerantParallelClient<ServerDetails, SomeSpecificException>
                         dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test

        let retrievedData =
            client
                .Query settings
                       [ func1; func2; func3 ]
                    |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<decimal>())
        Assert.That(retrievedData, Is.EqualTo someBalance)

    [<Test>]
    member __.``test new consistency setting designed to take advantage of caching (II - cache obsolete)``() =
        let someBalance = 1.0m
        let someBalanceMatchFunc someRetrievedBalance =
            someRetrievedBalance = someBalance

        let consistencyConfig = OneServerConsistentWithCertainValueOrTwoServers someBalanceMatchFunc |> Some
        let settings = defaultSettingsForNoConsistencyNoParallelismAndNoRetries consistencyConfig

        let newBalance = 2.0m
        let wrongBalance = 3.0m
        let otherWrongBalance = 4.0m

        let aJob1 =
            async { return wrongBalance }
        let aJob2 =
            async { return newBalance }
        let aJob3 =
            async { return otherWrongBalance }
        let aJob4 =
            async { return newBalance }

        let func1,func2,func3,func4 = serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob1" aJob1,
                                      serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob2" aJob2,
                                      serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob3" aJob3,
                                      serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob4" aJob4

        let client = FaultTolerantParallelClient<ServerDetails, SomeSpecificException>
                         dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test

        let retrievedData =
            client
                .Query settings
                       [ func1; func2; func3; func4 ]
                    |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<decimal>())
        Assert.That(retrievedData, Is.EqualTo newBalance)


    [<Test>]
    member __.``retries at least once if all fail``() =
        let mutable count1 = 0
        let aJob1 = async {
            count1 <- count1 + 1
            if (count1 = 1) then
                raise SomeException
            return 0
        }
        let mutable count2 = 0
        let aJob2 = async {
            count2 <- count2 + 1
            if (count2 = 1) then
                raise SomeException
            return 0
        }

        let func1,func2 = serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob1" aJob1,
                          serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob2" aJob2

        let settings = { defaultSettingsForNoConsistencyNoParallelismAndNoRetries None with
                            NumberOfRetries = 1u
                            NumberOfRetriesForInconsistency = 0u }

        let client = FaultTolerantParallelClient<ServerDetails, SomeException>
                         dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test
        client.Query settings
                     [ func1; func2 ]
            |> Async.RunSynchronously
            // enough to know that it doesn't throw
            |> ignore<int>

    [<Test>]
    member __.``it retries before throwing inconsistency exception``() =
        let numberOfConsistentResponsesToBeConsideredSafe = 3u

        let mostConsistentResult = 1
        let someOtherResultA = 2
        let someOtherResultB = 3

        let aJob1 =
            async { return someOtherResultA }
        let aJob2 =
            async { return mostConsistentResult }

        let mutable countA = 0
        let aJob3whichGetsConsistentAtSecondTry = async {
            countA <- countA + 1
            if (countA = 1) then
                return someOtherResultB
            else
                return mostConsistentResult
        }

        let mutable countB = 0
        let aJob3whichGetsConsistentAtThirdTry = async {
            countB <- countB + 1
            if (countB < 3) then
                return someOtherResultB
            else
                return mostConsistentResult
        }
        let aJob4 =
            async { return mostConsistentResult }

        let consistencyCfg =
            SpecificNumberOfConsistentResponsesRequired numberOfConsistentResponsesToBeConsideredSafe |> Some
        let settings =
            {
                defaultSettingsForNoConsistencyNoParallelismAndNoRetries consistencyCfg with
                    NumberOfRetries = 0u
                    NumberOfRetriesForInconsistency = 1u
            }

        let client = FaultTolerantParallelClient<ServerDetails, SomeSpecificException>
                         dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test

        let func1,func2,func3whichGetsConsistentAtSecondTry,func3whichGetsConsistentAtThirdTry,func4 =
            serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob1" aJob1,
            serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob2" aJob2,
            serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob3-2" aJob3whichGetsConsistentAtSecondTry,
            serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob3-3" aJob3whichGetsConsistentAtThirdTry,
            serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob4" aJob4

        client.Query
            settings
            [ func1; func2; func3whichGetsConsistentAtSecondTry; func4 ]
                |> Async.RunSynchronously
                |> ignore<int>

        let inconsistencyEx = Assert.Throws<ResultInconsistencyException>(fun _ ->
                                  client.Query
                                      settings
                                      [ func1; func2; func3whichGetsConsistentAtThirdTry; func4 ]
                                          |> Async.RunSynchronously
                                          |> ignore<int> )
        Assert.That(inconsistencyEx.Message, Is.StringContaining("received: 4, consistent: 2, required: 3"))

    [<Test>]
    member __.``using an average func instead of consistency``() =
        let job1 =
            async { return 1 }
        let job2 =
            async { return 5 }
        let job3 =
            async { return 6 }

        let funcs = [ serverWithNoHistoryInfoBecauseIrrelevantToThisTest "job1" job1
                      serverWithNoHistoryInfoBecauseIrrelevantToThisTest "job2" job2
                      serverWithNoHistoryInfoBecauseIrrelevantToThisTest "job3" job3 ]

        let consistencyCfg = AverageBetweenResponses (uint32 funcs.Length,
                                                      (fun (list:List<int>) ->
                                                         list.Sum() / list.Length
                                                      )) |> Some
        let settings =
            {
                defaultSettingsForNoConsistencyNoParallelismAndNoRetries consistencyCfg with
                    NumberOfParallelJobsAllowed = uint32 funcs.Length
            }

        let client = FaultTolerantParallelClient<ServerDetails, SomeSpecificException>
                         dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test

        let result = client.Query
                         settings
                         funcs
                             |> Async.RunSynchronously

        Assert.That(result, Is.EqualTo ((1+5+6)/3))

    [<Test>]
    member __.``ordering: chooses server with no faults first``() =
        let someResult1 = 1
        let someResult2 = 2
        let fault = some_fault_with_no_last_successful_comm_because_irrelevant_for_this_test
        let server1 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server1"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory =
                                      Some ({ Status = fault; TimeSpan = TimeSpan.FromSeconds 1.0 },
                                            dummy_date_for_cache)
                              }
                          Retrieval = async { return someResult1 }
                      }
        let server2 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server2"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = Some ({ Status = Success
                                                                 TimeSpan = TimeSpan.FromSeconds 2.0 },
                                                                dummy_date_for_cache)
                              }
                          Retrieval = async { return someResult2 }
                      }
        let retrievedData = (FaultTolerantParallelClient<ServerDetails,DummyIrrelevantToThisTestException>
                                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test).Query
                                (FaultTolerance.DefaultSettingsForNoConsistencyNoParallelismAndNoRetries None)
                                [ server1; server2 ]
                                |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someResult2)

        // same but different order
        let retrievedData = (FaultTolerantParallelClient<ServerDetails, DummyIrrelevantToThisTestException>
                                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test).Query
                                (FaultTolerance.DefaultSettingsForNoConsistencyNoParallelismAndNoRetries None)
                                [ server2; server1 ]
                                |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someResult2)

    [<Test>]
    member __.``ordering: chooses fastest fail-server option first``() =
        let someResult1 = 1
        let someResult2 = 2
        let fault = some_fault_with_no_last_successful_comm_because_irrelevant_for_this_test
        let server1 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server1"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = Some ({ Status = fault; TimeSpan = TimeSpan.FromSeconds 2.0 },
                                                               dummy_date_for_cache)
                              }
                          Retrieval = async { return someResult1 }
                      }
        let server2 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server2"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = Some ({ Status = fault; TimeSpan = TimeSpan.FromSeconds 1.0 },
                                                               dummy_date_for_cache)
                              }
                          Retrieval = async { return someResult2 }
                      }
        let retrievedData = (FaultTolerantParallelClient<ServerDetails, DummyIrrelevantToThisTestException>
                                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test).Query
                                (FaultTolerance.DefaultSettingsForNoConsistencyNoParallelismAndNoRetries None)
                                [ server1; server2 ]
                                |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someResult2)

        // same but different order
        let retrievedData = (FaultTolerantParallelClient<ServerDetails, DummyIrrelevantToThisTestException>
                                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test).Query
                                (FaultTolerance.DefaultSettingsForNoConsistencyNoParallelismAndNoRetries None)
                                [ server2; server1 ]
                                |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someResult2)

    [<Test>]
    member __.``ordering: chooses server with no faults over servers with no history``() =
        let someResult1 = 1
        let someResult2 = 2
        let server1 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server1"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = Some ({ Status = Success
                                                                 TimeSpan = TimeSpan.FromSeconds 2.0 },
                                                               dummy_date_for_cache)
                              }
                          Retrieval = async { return someResult1 }
                      }
        let server2 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server2"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = None
                              }
                          Retrieval = async { return someResult2 }
                      }
        let retrievedData = (FaultTolerantParallelClient<ServerDetails, DummyIrrelevantToThisTestException>
                                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test).Query
                                (FaultTolerance.DefaultSettingsForNoConsistencyNoParallelismAndNoRetries None)
                                [ server1; server2 ]
                                    |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someResult1)

        // same but different order
        let retrievedData = (FaultTolerantParallelClient<ServerDetails, DummyIrrelevantToThisTestException>
                                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test).Query
                                (FaultTolerance.DefaultSettingsForNoConsistencyNoParallelismAndNoRetries None)
                                [ server2; server1 ]
                                |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someResult1)

    [<Test>]
    member __.``ordering: chooses server with no history before servers with faults in their history``() =
        let someResult1 = 1
        let someResult2 = 2
        let fault = some_fault_with_no_last_successful_comm_because_irrelevant_for_this_test
        let server1 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server1"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = Some ({ Status = fault; TimeSpan = TimeSpan.FromSeconds 1.0 },
                                                               dummy_date_for_cache)
                              }
                          Retrieval = async { return someResult1 }
                      }
        let server2 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server2"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = None
                              }
                          Retrieval = async { return someResult2 }
                      }
        let retrievedData = (FaultTolerantParallelClient<ServerDetails, DummyIrrelevantToThisTestException>
                                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test).Query
                                (FaultTolerance.DefaultSettingsForNoConsistencyNoParallelismAndNoRetries None)
                                [ server1; server2 ]
                                |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someResult2)

        // same but different order
        let retrievedData = (FaultTolerantParallelClient<ServerDetails, DummyIrrelevantToThisTestException>
                                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test).Query
                                (FaultTolerance.DefaultSettingsForNoConsistencyNoParallelismAndNoRetries None)
                                [ server2; server1 ]
                                |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someResult2)

    [<Test>]
    member __.``ordering: chooses server with no history before any other servers, in analysis(non fast) mode``() =
        let someResult1 = 1
        let someResult2 = 2
        let someResult3 = 3
        let fault = some_fault_with_no_last_successful_comm_because_irrelevant_for_this_test
        let server1 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server1"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = Some ({ Status = fault; TimeSpan = TimeSpan.FromSeconds 1.0 },
                                                               dummy_date_for_cache)
                              }
                          Retrieval = async { return someResult1 }
                      }
        let server2 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server2"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = None
                              }
                          Retrieval = async { return someResult2 }
                      }
        let server3 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server3"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = Some({ Status = Success
                                                                TimeSpan = TimeSpan.FromSeconds 1.0 },
                                                              dummy_date_for_cache)
                              }
                          Retrieval = async { return someResult3 }
                      }

        let defaultSettings = FaultTolerance.DefaultSettingsForNoConsistencyNoParallelismAndNoRetries None
        let settings =
            match defaultSettings.ResultSelectionMode with
            | Selective selSettings ->
                {
                    defaultSettings with
                        ResultSelectionMode =
                            Selective
                                {
                                    selSettings with
                                        ServerSelectionMode = ServerSelectionMode.Analysis
                                }
                }
            | _ -> failwith "default settings should be selective! :-?"
        let retrievedData = (FaultTolerantParallelClient<ServerDetails, DummyIrrelevantToThisTestException>
                                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test).Query
                                settings
                                [ server1; server2; server3 ]
                                |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someResult2)

        // same but different order
        let retrievedData = (FaultTolerantParallelClient<ServerDetails, DummyIrrelevantToThisTestException>
                                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test).Query
                                settings
                                [ server3; server2; server1 ]
                                |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someResult2)

    [<Test>]
    member __.``ordering: leaves one third of servers queried for faulty ones in analysis(non-fast) mode``() =
        let someResult3 = 3
        let someResult4 = 4
        let server1 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server1"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = Some({ Status = Success
                                                                TimeSpan = TimeSpan.FromSeconds 1.0 },
                                                              dummy_date_for_cache)
                              }
                          Retrieval = async { return raise SomeSpecificException }
                      }
        let server2 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server2"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = Some({ Status = Success
                                                                TimeSpan = TimeSpan.FromSeconds 2.0 },
                                                              dummy_date_for_cache)
                              }
                          Retrieval = async { return raise SomeSpecificException }
                      }
        let server3 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server3"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = Some({ Status = Success
                                                                TimeSpan = TimeSpan.FromSeconds 3.0 },
                                                              dummy_date_for_cache)
                              }
                          Retrieval = async { return someResult3 }
                      }
        let fault = some_fault_with_no_last_successful_comm_because_irrelevant_for_this_test
        let server4 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server4"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = Some({ Status = fault
                                                                TimeSpan = TimeSpan.FromSeconds 1.0 },
                                                              dummy_date_for_cache)
                              }
                          Retrieval = async { return someResult4 }
                      }


        let defaultSettings = FaultTolerance.DefaultSettingsForNoConsistencyNoParallelismAndNoRetries None
        let settings =
            match defaultSettings.ResultSelectionMode with
            | Selective selSettings ->
                {
                    defaultSettings with
                        ResultSelectionMode =
                            Selective
                                {
                                    selSettings with
                                        ServerSelectionMode = ServerSelectionMode.Analysis
                                }
                }
            | _ -> failwith "default settings should be selective! :-?"
        let retrievedData = (FaultTolerantParallelClient<ServerDetails, SomeSpecificException>
                                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test).Query
                                settings
                                [ server1; server2; server3; server4 ]
                                |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someResult4)

        // same but different order
        let retrievedData = (FaultTolerantParallelClient<ServerDetails, SomeSpecificException>
                                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test).Query
                                settings
                                [ server4; server3; server2; server1 ]
                                |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someResult4)

    [<Test>]
    member __.``ordering: leaves every 4th position for a non-best server in analysis(non-fast) mode``() =
        let someResult4 = 4
        let someResult5 = 5
        let server1 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server1"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = Some({ Status = Success
                                                                TimeSpan = TimeSpan.FromSeconds 1.0 },
                                                              dummy_date_for_cache)
                              }
                          Retrieval = async { return raise SomeSpecificException }
                      }
        let server2 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server2"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = Some({ Status = Success
                                                                TimeSpan = TimeSpan.FromSeconds 2.0 },
                                                              dummy_date_for_cache)
                              }
                          Retrieval = async { return raise SomeSpecificException }
                      }
        let server3 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server3"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = Some({ Status = Success
                                                                TimeSpan = TimeSpan.FromSeconds 3.0 },
                                                              dummy_date_for_cache)
                              }
                          Retrieval = async { return raise SomeSpecificException }
                      }

        let server4 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server4"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = Some({ Status = Success
                                                                TimeSpan = TimeSpan.FromSeconds 4.0 },
                                                              dummy_date_for_cache)
                              }
                          Retrieval = async { return someResult4 }
                      }
        let server5 = {
                          Details =
                              {
                                  ServerInfo =
                                      {
                                          NetworkPath = "server5"
                                          ConnectionType = dummy_connection_type
                                      }
                                  CommunicationHistory = Some({ Status = Success
                                                                TimeSpan = TimeSpan.FromSeconds 5.0 },
                                                              dummy_date_for_cache)
                              }
                          Retrieval = async { return someResult5 }
                      }

        let defaultSettings = FaultTolerance.DefaultSettingsForNoConsistencyNoParallelismAndNoRetries None
        let settings =
            match defaultSettings.ResultSelectionMode with
            | Selective selSettings ->
                {
                    defaultSettings with
                        ResultSelectionMode =
                            Selective
                                {
                                    selSettings with
                                        ServerSelectionMode = ServerSelectionMode.Analysis
                                }
                }
            | _ -> failwith "default settings should be selective! :-?"

        let retrievedData = (FaultTolerantParallelClient<ServerDetails, SomeSpecificException>
                                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test).Query
                                settings
                                [ server1; server2; server3; server4; server5 ]
                                |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someResult5)

        // same but different order
        let retrievedData = (FaultTolerantParallelClient<ServerDetails, SomeSpecificException>
                                dummy_func_to_not_save_server_because_it_is_irrelevant_for_this_test).Query
                                settings
                                [ server5; server4; server3; server2; server1 ]
                                |> Async.RunSynchronously
        Assert.That(retrievedData, Is.TypeOf<int>())
        Assert.That(retrievedData, Is.EqualTo someResult5)

    [<Test>]
    member __.``can save server last stat``() =
        let someResult = 1
        let aJob =
            async { return someResult }
        let serverId = "aJob"
        let func = serverWithNoHistoryInfoBecauseIrrelevantToThisTest serverId aJob

        let mutable someFlag = false
        let saveServerLastStat (isServer: ServerDetails->bool) (historyFact: HistoryFact): unit =
            Assert.That(isServer func.Details, Is.EqualTo true)
            match historyFact.Fault with
            | Some _ ->
                failwith "assertion failed"
            | _ ->
                ()
            Assert.That(historyFact.TimeSpan, Is.GreaterThan TimeSpan.Zero)
            someFlag <- true

        (FaultTolerantParallelClient<ServerDetails, DummyIrrelevantToThisTestException>
                                    saveServerLastStat).Query
                (defaultSettingsForNoConsistencyNoParallelismAndNoRetries None) [ func ]
                |> Async.RunSynchronously
                |> ignore<int>

        Assert.That(someFlag, Is.EqualTo true)

    [<Test>]
    member __.``can save server last fault``() =
        let aFailingJob: Async<int> =
            async { return raise SomeSpecificException }
        let failingServerName = "aFailingJob"
        let server1 = serverWithNoHistoryInfoBecauseIrrelevantToThisTest failingServerName aFailingJob

        let someResult = 1
        let aJob =
            async { return someResult }
        let server2 = serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob" aJob

        let mutable someNonFailingCounter = 0
        let mutable someTotalCounter = 0
        let lockObj = Object()
        let saveServerLastStat (isServer: ServerDetails->bool) (historyFact: HistoryFact): unit =
            lock lockObj (fun _ ->
                match historyFact.Fault with
                | Some ex ->
                    Assert.That(isServer server1.Details, Is.EqualTo true)
                    Assert.That(isServer server2.Details, Is.EqualTo false)
                    Assert.That(ex.TypeFullName, Is.EqualTo typeof<SomeSpecificException>.FullName)
                | _ ->
                    Assert.That(isServer server1.Details, Is.EqualTo false)
                    Assert.That(isServer server2.Details, Is.EqualTo true)
                    someNonFailingCounter <- someNonFailingCounter + 1

                Assert.That(historyFact.TimeSpan, Is.GreaterThan TimeSpan.Zero)
                someTotalCounter <- someTotalCounter + 1
            )

        (FaultTolerantParallelClient<ServerDetails, SomeSpecificException>
                                    saveServerLastStat).Query
                (defaultSettingsForNoConsistencyNoParallelismAndNoRetries None)
                [ server1; server2 ]
                |> Async.RunSynchronously
                |> ignore<int>

        Assert.That(someTotalCounter, Is.EqualTo 2)
        Assert.That(someNonFailingCounter, Is.EqualTo 1)

    [<Test>]
    member __.``calls all jobs in exhaustive mode``() =
        let someResult = 1
        let mutable aJob1Called = false
        let aJob1 =
            async { aJob1Called <- true; return someResult }
        let mutable aJob2Called = false
        let aJob2 =
            async { aJob2Called <- true; return someResult }
        let func1,func2 = serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob1" aJob1,
                          serverWithNoHistoryInfoBecauseIrrelevantToThisTest "aJob2" aJob2

        let settings =
            {
                NumberOfParallelJobsAllowed = not_more_than_one_parallel_job_because_this_test_doesnt_test_parallelization
                NumberOfRetries = test_does_not_involve_retries
                NumberOfRetriesForInconsistency = test_does_not_involve_retries
                ResultSelectionMode = ResultSelectionMode.Exhaustive
                ExceptionHandler = None
            }
        let retrievedData1 = defaultFaultTolerantParallelClient.Query
                                settings
                                [ func1; func2 ]
                                    |> Async.RunSynchronously
        Assert.That(retrievedData1, Is.TypeOf<int>())
        Assert.That(retrievedData1, Is.EqualTo someResult)
        Assert.That(aJob1Called, Is.EqualTo true)
        Assert.That(aJob2Called, Is.EqualTo true)

        aJob1Called <- false
        aJob2Called <- false
        // different order
        let retrievedData2 = defaultFaultTolerantParallelClient.Query
                                settings
                                [ func2; func1 ]
                                    |> Async.RunSynchronously
        Assert.That(retrievedData2, Is.TypeOf<int>())
        Assert.That(retrievedData2, Is.EqualTo someResult)
        Assert.That(aJob1Called, Is.EqualTo true)
        Assert.That(aJob2Called, Is.EqualTo true)

    [<Test>]
    member __.``low server count does not cause exception``() =
        let someResult = 1
        let mutable singleJobCalled = false
        let singleJob =
            async { singleJobCalled <- true; return someResult }
        let func1 = serverWithNoHistoryInfoBecauseIrrelevantToThisTest "singleJob" singleJob

        let settings =
            {
                NumberOfParallelJobsAllowed = two_because_its_larger_than_the_length_of_the_server_list_which_has_one_elem
                NumberOfRetries = test_does_not_involve_retries
                NumberOfRetriesForInconsistency = test_does_not_involve_retries
                ResultSelectionMode = ResultSelectionMode.Exhaustive
                ExceptionHandler = None
            }
        // test that it doesn't throw
        let retrievedData = defaultFaultTolerantParallelClient.Query
                                settings
                                [ func1 ]
                                    |> Async.RunSynchronously

        Assert.That(singleJobCalled, Is.EqualTo true)
        Assert.That(retrievedData, Is.EqualTo someResult)

    member private __.DefaultSettingsForNoConsistencyNoParallelismAndNoRetries consistencyConfig =
        defaultSettingsForNoConsistencyNoParallelismAndNoRetries consistencyConfig

    static member DefaultSettingsForNoConsistencyNoParallelismAndNoRetries consistencyConfig =
        FaultTolerance().DefaultSettingsForNoConsistencyNoParallelismAndNoRetries consistencyConfig
