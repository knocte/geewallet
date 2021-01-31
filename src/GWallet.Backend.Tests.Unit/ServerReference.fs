namespace GWallet.Backend.Tests.Unit

open System

open Newtonsoft.Json
open NUnit.Framework

open GWallet.Backend

[<TestFixture>]
type ServerReference() =

    let dummy_currency_because_irrelevant_for_this_test = Currency.BTC
    let dummy_now = DateTime.UtcNow
    let some_connection_type_irrelevant_for_this_test = { Encrypted = false; Protocol = Http }

    let CreateHistoryInfoWithLsc(lastSuccessfulCommunication: DateTime) =
        ({
            Status = Success

            //irrelevant for this test
            TimeSpan = TimeSpan.Zero

        },lastSuccessfulCommunication) |> Some

    let CreateHistoryInfoWithSpan(timeSpan: TimeSpan) =
        ({
            //irrelevant for this test
            Status = Success

            TimeSpan = timeSpan
        },dummy_now) |> Some

    let CreateFaultyHistoryInfo (lastSuccessfulCommunication: DateTime) =
        ({
            // exception info irrelevant for this test
            Status = Fault { Exception = { TypeFullName = "SomeNamespace.SomeException" ; Message = "argh" }
                             LastSuccessfulCommunication = None }

            //irrelevant for this test
            TimeSpan = TimeSpan.Zero
        },lastSuccessfulCommunication) |> Some

    let CreateFaultyHistoryInfoWithSpan(timeSpan: TimeSpan) =
        ({
            //irrelevant for this test
            Status = Fault { Exception = { TypeFullName = "SomeNamespace.SomeException" ; Message = "argh" }
                             LastSuccessfulCommunication = None }

            TimeSpan = timeSpan
        },dummy_now) |> Some

    [<Test>]
    member __.``order of servers is kept if non-hostname details are same``() =
        let serverWithHighestPriority =
            {
                ServerInfo =
                    {
                        NetworkPath = "dlm8yerwlcifs"
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = None
            }
        let serverWithLowestPriority =
            {
                ServerInfo =
                    {
                        NetworkPath = "eliuh4midkndk"
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = None
            }
        let servers1 = Map.empty.Add
                                (dummy_currency_because_irrelevant_for_this_test,
                                seq { yield serverWithHighestPriority; yield serverWithLowestPriority })
        let serverDetails = ServerRegistry.Serialize servers1

        let serverAPos = serverDetails.IndexOf serverWithHighestPriority.ServerInfo.NetworkPath
        let serverBPos = serverDetails.IndexOf serverWithLowestPriority.ServerInfo.NetworkPath

        Assert.That(serverAPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.GreaterThan serverAPos, "shouldn't be sorted #1")

        let servers2 = Map.empty.Add
                                (dummy_currency_because_irrelevant_for_this_test,
                                seq { yield serverWithLowestPriority; yield serverWithHighestPriority })
        let serverDetailsReverse = ServerRegistry.Serialize servers2

        let serverAPos = serverDetailsReverse.IndexOf serverWithHighestPriority.ServerInfo.NetworkPath
        let serverBPos = serverDetailsReverse.IndexOf serverWithLowestPriority.ServerInfo.NetworkPath

        Assert.That(serverAPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.Not.LessThan 0)

        Assert.That(serverAPos, Is.GreaterThan serverBPos, "shouldn't be sorted #2")

    [<Test>]
    member __.``order of servers: no history comes last``() =
        let serverWithRecentConnectionInfo =
            {
                ServerInfo =
                    {
                        NetworkPath = "pkine34o4hirn"
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = CreateHistoryInfoWithLsc DateTime.Now
            }
        let serverWithNoLastConnection =
            {
                ServerInfo =
                    {
                        NetworkPath = "dlm8yerwlcifs"
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = None
            }

        let servers1 = Map.empty.Add
                                (dummy_currency_because_irrelevant_for_this_test,
                                seq { yield serverWithNoLastConnection; yield serverWithRecentConnectionInfo })
        let serverDetails1 = ServerRegistry.Serialize servers1

        let serverAPos = serverDetails1.IndexOf serverWithNoLastConnection.ServerInfo.NetworkPath
        let serverBPos = serverDetails1.IndexOf serverWithRecentConnectionInfo.ServerInfo.NetworkPath

        Assert.That(serverAPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.Not.LessThan 0)

        Assert.That(serverAPos, Is.GreaterThan serverBPos, "should be sorted #1")

        let servers2 = Map.empty.Add
                                (dummy_currency_because_irrelevant_for_this_test,
                                seq { yield serverWithRecentConnectionInfo; yield serverWithNoLastConnection })
        let serverDetails2 = ServerRegistry.Serialize servers2

        let serverAPos = serverDetails2.IndexOf serverWithNoLastConnection.ServerInfo.NetworkPath
        let serverBPos = serverDetails2.IndexOf serverWithRecentConnectionInfo.ServerInfo.NetworkPath

        Assert.That(serverAPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.Not.LessThan 0)

        Assert.That(serverAPos, Is.GreaterThan serverBPos, "should be sorted #2")

    [<Test>]
    member __.``order of servers depends on last successful conn``() =
        let serverWithOldestConnection =
            {
                ServerInfo =
                    {
                        NetworkPath = "dlm8yerwlcifs"
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = CreateHistoryInfoWithLsc (DateTime.Now - TimeSpan.FromDays 10.0)
            }
        let serverWithMostRecentConnection =
            {
                ServerInfo =
                    {
                        NetworkPath = "eliuh4midkndk"
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = CreateHistoryInfoWithLsc DateTime.Now
            }
        let servers1 = Map.empty.Add
                                (dummy_currency_because_irrelevant_for_this_test,
                                seq { yield serverWithOldestConnection; yield serverWithMostRecentConnection })
        let serverDetails = ServerRegistry.Serialize servers1

        let serverAPos = serverDetails.IndexOf serverWithOldestConnection.ServerInfo.NetworkPath
        let serverBPos = serverDetails.IndexOf serverWithMostRecentConnection.ServerInfo.NetworkPath

        Assert.That(serverAPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.Not.LessThan 0)

        Assert.That(serverAPos, Is.GreaterThan serverBPos, "should be sorted #1")

        let servers2 = Map.empty.Add
                                (dummy_currency_because_irrelevant_for_this_test,
                                seq { yield serverWithMostRecentConnection; yield serverWithOldestConnection })
        let serverDetailsReverse = ServerRegistry.Serialize servers2

        let serverAPos = serverDetailsReverse.IndexOf serverWithOldestConnection.ServerInfo.NetworkPath
        let serverBPos = serverDetailsReverse.IndexOf serverWithMostRecentConnection.ServerInfo.NetworkPath

        Assert.That(serverAPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.Not.LessThan 0)

        Assert.That(serverAPos, Is.GreaterThan serverBPos, "should be sorted #2")

    [<Test>]
    member __.``order of servers depends on shortest timespan``() =
        let serverWithWorstConnectionSpan =
            {
                ServerInfo =
                    {
                        NetworkPath = "mxiunrciunri"
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = CreateHistoryInfoWithSpan (TimeSpan.FromSeconds 5.0)
            }
        let serverWithBestConnectionSpan =
            {
                ServerInfo =
                    {
                        NetworkPath = "ekuyzegnuyen"
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = CreateHistoryInfoWithSpan (TimeSpan.FromSeconds 1.0)
            }
        let servers1 = Map.empty.Add
                                (dummy_currency_because_irrelevant_for_this_test,
                                seq { yield serverWithWorstConnectionSpan; yield serverWithBestConnectionSpan })
        let serverDetails = ServerRegistry.Serialize servers1

        let serverAPos = serverDetails.IndexOf serverWithWorstConnectionSpan.ServerInfo.NetworkPath
        let serverBPos = serverDetails.IndexOf serverWithBestConnectionSpan.ServerInfo.NetworkPath

        Assert.That(serverAPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.Not.LessThan 0)

        Assert.That(serverAPos, Is.GreaterThan serverBPos, "should be sorted #1")

        let servers2 = Map.empty.Add
                                (dummy_currency_because_irrelevant_for_this_test,
                                seq { yield serverWithBestConnectionSpan; yield serverWithWorstConnectionSpan })
        let serverDetailsReverse = ServerRegistry.Serialize servers2

        let serverAPos = serverDetailsReverse.IndexOf serverWithWorstConnectionSpan.ServerInfo.NetworkPath
        let serverBPos = serverDetailsReverse.IndexOf serverWithBestConnectionSpan.ServerInfo.NetworkPath

        Assert.That(serverAPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.Not.LessThan 0)

        Assert.That(serverAPos, Is.GreaterThan serverBPos, "should be sorted #2")

    [<Test>]
    member __.``order of servers: faulty ones come after successful ones``() =
        let serverWithBadConnectionSpan =
            {
                ServerInfo =
                    {
                        NetworkPath = "mxiunrciunri"
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = CreateFaultyHistoryInfoWithSpan (TimeSpan.FromSeconds 0.5)
            }
        let serverWithGoodConnectionSpan =
            {
                ServerInfo =
                    {
                        NetworkPath = "ekuyzegnuyen"
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = CreateHistoryInfoWithSpan (TimeSpan.FromSeconds 1.0)
            }
        let servers1 = Map.empty.Add
                                (dummy_currency_because_irrelevant_for_this_test,
                                seq { yield serverWithBadConnectionSpan; yield serverWithGoodConnectionSpan })
        let serverDetails = ServerRegistry.Serialize servers1

        let serverAPos = serverDetails.IndexOf serverWithBadConnectionSpan.ServerInfo.NetworkPath
        let serverBPos = serverDetails.IndexOf serverWithGoodConnectionSpan.ServerInfo.NetworkPath

        Assert.That(serverAPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.Not.LessThan 0)

        Assert.That(serverAPos, Is.GreaterThan serverBPos, "should be sorted #1")

        let servers2 = Map.empty.Add
                                (dummy_currency_because_irrelevant_for_this_test,
                                seq { yield serverWithGoodConnectionSpan; yield serverWithBadConnectionSpan })
        let serverDetailsReverse = ServerRegistry.Serialize servers2

        let serverAPos = serverDetailsReverse.IndexOf serverWithBadConnectionSpan.ServerInfo.NetworkPath
        let serverBPos = serverDetailsReverse.IndexOf serverWithGoodConnectionSpan.ServerInfo.NetworkPath

        Assert.That(serverAPos, Is.Not.LessThan 0)

        Assert.That(serverBPos, Is.Not.LessThan 0)

        Assert.That(serverAPos, Is.GreaterThan serverBPos, "should be sorted #2")

    [<Test>]
    member __.``stats of server are included in serialization``() =
        let now = DateTime.UtcNow
        let serverWithSomeRecentConnection =
            {
                ServerInfo =
                    {
                        NetworkPath = "eliuh4midkndk"
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = CreateHistoryInfoWithLsc now
            }
        let servers = Map.empty.Add
                                (dummy_currency_because_irrelevant_for_this_test,
                                seq { yield serverWithSomeRecentConnection })
        let serverDetails = ServerRegistry.Serialize servers

        let dayPos = serverDetails.IndexOf (now.Day.ToString())
        Assert.That(dayPos, Is.GreaterThan 0)

        let monthPos = serverDetails.IndexOf (now.Month.ToString())
        Assert.That(monthPos, Is.GreaterThan 0)

        let yearPos = serverDetails.IndexOf (now.Year.ToString())
        Assert.That(yearPos, Is.GreaterThan 0)

        let hourPos = serverDetails.IndexOf (now.Hour.ToString())
        Assert.That(hourPos, Is.GreaterThan 0)

        let minPos = serverDetails.IndexOf (now.Minute.ToString())
        Assert.That(minPos, Is.GreaterThan 0)

    [<Test>]
    member __.``serialization is JSON based (for readability and consistency with rest of wallet)``() =
        let serverWithSomeRecentConnection =
            {
                ServerInfo =
                    {
                        NetworkPath = "eliuh4midkndk"
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = CreateHistoryInfoWithLsc DateTime.UtcNow
            }
        let servers = Map.empty.Add
                                (dummy_currency_because_irrelevant_for_this_test,
                                seq { yield serverWithSomeRecentConnection })
        let serverDetails = ServerRegistry.Serialize servers

        let deserializedServerDetails = JsonConvert.DeserializeObject serverDetails
        Assert.That(deserializedServerDetails, Is.Not.Null)

    [<Test>]
    member __.``details of server are included in serialization``() =
        let port = 50001u
        let serverWithSomeRecentConnection =
            {
                ServerInfo =
                    {
                        NetworkPath = "eliuh4midkndk"
                        ConnectionType = { Encrypted = false; Protocol = Tcp port }
                    }
                CommunicationHistory = None
             }
        let servers = Map.empty.Add
                                (dummy_currency_because_irrelevant_for_this_test,
                                seq { yield serverWithSomeRecentConnection })
        let serverDetails = ServerRegistry.Serialize servers

        let portPos = serverDetails.IndexOf (port.ToString())
        Assert.That(portPos, Is.GreaterThan 0)

    [<Test>]
    member __.``serializing and deserializing leads to same result (no order regarded in this test)``() =
        let tcpServerNetworkPath = "tcp"
        let tcpServerWithNoHistory =
            {
                ServerInfo =
                    {
                        NetworkPath = tcpServerNetworkPath
                        ConnectionType = { Encrypted = false; Protocol = Tcp 50001u }
                    }
                CommunicationHistory = None
            }

        let timeSpanForHttpServer = TimeSpan.FromSeconds 1.0
        let httpServerNetworkPath = "http"
        let lastSuccessfulCommunication = DateTime.UtcNow
        let httpSuccessfulServer =
            {
                ServerInfo =
                    {
                        NetworkPath = httpServerNetworkPath
                        ConnectionType = { Encrypted = false; Protocol = Http }
                    }
                CommunicationHistory = Some({
                                                Status = Success
                                                TimeSpan = timeSpanForHttpServer
                                            },
                                            lastSuccessfulCommunication)
             }

        let httpsServerNetworkPath1 = "https1"
        let timeSpanForHttpsServer = TimeSpan.FromSeconds 2.0
        let exInfo = { TypeFullName = "SomeNamespace.SomeException" ; Message = "argh" }
        let httpsFailureServer1 =
            {
                ServerInfo =
                    {
                        NetworkPath = httpsServerNetworkPath1
                        ConnectionType = { Encrypted = true; Protocol = Http }
                    }
                CommunicationHistory = Some({
                                                Status = Fault { Exception = exInfo
                                                                 LastSuccessfulCommunication = None }
                                                TimeSpan = timeSpanForHttpsServer
                                            },
                                            dummy_now)
             }
        let httpsServerNetworkPath2 = "https2"
        let httpsFailureServer2 =
            {
                ServerInfo =
                    {
                        NetworkPath = httpsServerNetworkPath2
                        ConnectionType = { Encrypted = true; Protocol = Http }
                    }
                CommunicationHistory = Some({
                                                Status =
                                                    Fault
                                                        {
                                                            Exception = exInfo
                                                            LastSuccessfulCommunication =
                                                                Some lastSuccessfulCommunication
                                                        }
                                                TimeSpan = timeSpanForHttpsServer
                                            },
                                            dummy_now)
             }

        let servers = Map.empty.Add
                                (dummy_currency_because_irrelevant_for_this_test,
                                 seq {
                                     yield tcpServerWithNoHistory
                                     yield httpSuccessfulServer
                                     yield httpsFailureServer1
                                     yield httpsFailureServer2
                                 })
        let serverDetails = ServerRegistry.Serialize servers

        let deserializedServers =
            ((ServerRegistry.Deserialize serverDetails).TryFind dummy_currency_because_irrelevant_for_this_test).Value
                |> List.ofSeq
        Assert.That(deserializedServers.Length, Is.EqualTo 4)

        let tcpServers = Seq.filter (fun server -> server.ServerInfo.NetworkPath = tcpServerNetworkPath)
                                    deserializedServers
                                        |> List.ofSeq
        Assert.That(tcpServers.Length, Is.EqualTo 1)
        let tcpServer = tcpServers.[0]
        Assert.That(tcpServer.ServerInfo.NetworkPath, Is.EqualTo tcpServerNetworkPath)
        Assert.That(tcpServer.ServerInfo.ConnectionType.Encrypted, Is.EqualTo false)
        Assert.That(tcpServer.CommunicationHistory, Is.EqualTo None)

        let httpServers = Seq.filter (fun server -> server.ServerInfo.NetworkPath = httpServerNetworkPath)
                                      deserializedServers
                                          |> List.ofSeq
        Assert.That(httpServers.Length, Is.EqualTo 1)
        let httpServer = httpServers.[0]
        Assert.That(httpServer.ServerInfo.NetworkPath, Is.EqualTo httpServerNetworkPath)
        Assert.That(httpServer.ServerInfo.ConnectionType.Encrypted, Is.EqualTo false)
        match httpServer.CommunicationHistory with
        | None -> Assert.Fail "http server should have some historyinfo"
        | Some (historyInfo,lastComm) ->
            Assert.That(historyInfo.TimeSpan, Is.EqualTo timeSpanForHttpServer)
            match historyInfo.Status with
            | Fault _ ->
                Assert.Fail "http server should be successful, not failure"
            | Success ->
                Assert.That(lastComm, Is.EqualTo lastSuccessfulCommunication)

        let https1Servers = Seq.filter (fun server -> server.ServerInfo.NetworkPath = httpsServerNetworkPath1)
                                        deserializedServers
                                          |> List.ofSeq
        Assert.That(https1Servers.Length, Is.EqualTo 1)
        let httpsServer1 = https1Servers.[0]
        Assert.That(httpsServer1.ServerInfo.NetworkPath, Is.EqualTo httpsServerNetworkPath1)
        Assert.That(httpsServer1.ServerInfo.ConnectionType.Encrypted, Is.EqualTo true)
        match httpsServer1.CommunicationHistory with
        | None -> Assert.Fail "https server should have some historyinfo"
        | Some (historyInfo,_) ->
            Assert.That(historyInfo.TimeSpan, Is.EqualTo timeSpanForHttpsServer)
            match historyInfo.Status with
            | Fault faultInfo ->
                Assert.That(faultInfo.Exception.TypeFullName, Is.EqualTo exInfo.TypeFullName)
                Assert.That(faultInfo.Exception.Message, Is.EqualTo exInfo.Message)
                Assert.That(faultInfo.LastSuccessfulCommunication, Is.EqualTo None)
            | _ ->
                Assert.Fail "https server should be fault, not successful"

        let https2Servers = Seq.filter (fun server -> server.ServerInfo.NetworkPath = httpsServerNetworkPath2)
                                        deserializedServers
                                          |> List.ofSeq
        Assert.That(https2Servers.Length, Is.EqualTo 1)
        let httpsServer2 = https2Servers.[0]
        Assert.That(httpsServer2.ServerInfo.NetworkPath, Is.EqualTo httpsServerNetworkPath2)
        Assert.That(httpsServer2.ServerInfo.ConnectionType.Encrypted, Is.EqualTo true)
        match httpsServer2.CommunicationHistory with
        | None -> Assert.Fail "https server should have some historyinfo"
        | Some (historyInfo,_) ->
            Assert.That(historyInfo.TimeSpan, Is.EqualTo timeSpanForHttpsServer)
            match historyInfo.Status with
            | Fault faultInfo ->
                Assert.That(faultInfo.Exception.TypeFullName, Is.EqualTo exInfo.TypeFullName)
                Assert.That(faultInfo.Exception.Message, Is.EqualTo exInfo.Message)
                Assert.That(faultInfo.LastSuccessfulCommunication, Is.EqualTo (Some lastSuccessfulCommunication))
            | _ ->
                Assert.Fail "https server should be fault, not successful"

    [<Test>]
    member __.``duplicate servers are removed``() =
        let sameRandomHostname = "xfoihror3uo3wmio"
        let serverA =
            {
                ServerInfo =
                    {
                        NetworkPath = sameRandomHostname
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = None
            }
        let serverB =
            {
                ServerInfo =
                    {
                        NetworkPath = sameRandomHostname
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = CreateHistoryInfoWithLsc dummy_now
            }
        let servers = Map.empty.Add
                                (dummy_currency_because_irrelevant_for_this_test,
                                seq { yield serverA; yield serverB })
        let serverDetails = ServerRegistry.Serialize servers
        let deserializedServers =
            ((ServerRegistry.Deserialize serverDetails).TryFind dummy_currency_because_irrelevant_for_this_test).Value
                |> List.ofSeq

        Assert.That(deserializedServers.Length, Is.EqualTo 1)

    [<Test>]
    member __.``non-duplicate servers are not removed``() =
        let serverA =
            {
                ServerInfo =
                    {
                        NetworkPath = "A"
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = None
            }
        let serverB =
            {
                ServerInfo =
                    {
                        NetworkPath = "B"
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = None
            }

        let servers = Map.empty.Add
                                (dummy_currency_because_irrelevant_for_this_test, seq { yield serverA; yield serverB })
        let serverDetails = ServerRegistry.Serialize servers
        let deserializedServers =
            ((ServerRegistry.Deserialize serverDetails).TryFind dummy_currency_because_irrelevant_for_this_test).Value
                |> List.ofSeq

        Assert.That(deserializedServers.Length, Is.EqualTo 2)

    member private __.SerializeAndDeserialize (serverA: ServerDetails) (serverB: ServerDetails): List<ServerDetails> =
        let servers = seq { yield serverA; yield serverB }
        let serverRanking = Map.empty.Add (dummy_currency_because_irrelevant_for_this_test, servers)
        let serverDetails = ServerRegistry.Serialize serverRanking
        ((ServerRegistry.Deserialize serverDetails).TryFind dummy_currency_because_irrelevant_for_this_test).Value
            |> List.ofSeq

    member private __.Merge (serverA: ServerDetails) (serverB: ServerDetails): List<ServerDetails> =
        let serverRankingA =
            Map.empty.Add (dummy_currency_because_irrelevant_for_this_test, seq { yield serverA })
        let serverRankingB =
            Map.empty.Add (dummy_currency_because_irrelevant_for_this_test, seq { yield serverB })
        let mergedServerRanking = ServerRegistry.Merge serverRankingA serverRankingB
        (mergedServerRanking.TryFind dummy_currency_because_irrelevant_for_this_test)
            .Value
            |> List.ofSeq

    [<Test>]
    member self.``when removing duplicate servers, the ones with history and most up to date, stay (I)``() =
        let sameRandomHostname = "xfoihror3uo3wmio"
        let serverA =
            {
                ServerInfo =
                    {
                        NetworkPath = sameRandomHostname
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = None
            }
        let serverB =
            {
                ServerInfo =
                    {
                        NetworkPath = sameRandomHostname
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = CreateHistoryInfoWithLsc dummy_now
            }
        let deserializedServers = self.SerializeAndDeserialize serverA serverB

        Assert.That(deserializedServers.Length, Is.EqualTo 1)
        Assert.That(deserializedServers.[0].CommunicationHistory, Is.Not.EqualTo None)

        let mergedServers = self.Merge serverA serverB
        Assert.That(mergedServers.Length, Is.EqualTo 1)
        Assert.That(mergedServers.[0].CommunicationHistory, Is.Not.EqualTo None)

    [<Test>]
    member self.``when removing duplicate servers, the ones with history and most up to date, stay (II)``() =

        let olderDate = dummy_now - TimeSpan.FromDays 1.0
        let sameRandomHostname = "xfoihror3uo3wmio"
        let serverA =
            {
                ServerInfo =
                    {
                        NetworkPath = sameRandomHostname
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = CreateHistoryInfoWithLsc dummy_now
            }
        let serverB =
            {
                ServerInfo =
                    {
                        NetworkPath = sameRandomHostname
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = CreateHistoryInfoWithLsc olderDate
            }
        let deserializedServers = self.SerializeAndDeserialize serverA serverB
        let mergedServers = self.Merge serverA serverB

        Assert.That(deserializedServers.Length, Is.EqualTo 1)
        Assert.That(mergedServers.Length, Is.EqualTo 1)
        match deserializedServers.[0].CommunicationHistory,mergedServers.[0].CommunicationHistory with
        | Some (dHistory,dLastComm), Some (mHistory,mLastComm) ->
            match dHistory.Status,mHistory.Status with
            | Success,Success ->
                Assert.That(dLastComm, Is.EqualTo dummy_now)
                Assert.That(mLastComm, Is.EqualTo dummy_now)
            | _ -> Assert.Fail "both deserialized and merged should have status since both servers inserted had it #1"
        | _ ->
            Assert.Fail "both deserialized and merged should have some history since no server stored had None on it #1"

    [<Test>]
    member self.``when removing duplicate servers, the ones with history and most up to date, stay (III)``() =
        let olderDate = dummy_now - TimeSpan.FromDays 1.0
        let sameRandomHostname = "xfoihror3uo3wmio"
        let serverA =
            {
                ServerInfo =
                    {
                        NetworkPath = sameRandomHostname
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = CreateHistoryInfoWithLsc olderDate
            }
        let serverB =
            {
                ServerInfo =
                    {
                        NetworkPath = sameRandomHostname
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = CreateHistoryInfoWithLsc dummy_now
            }
        let deserializedServers = self.SerializeAndDeserialize serverA serverB
        let mergedServers = self.Merge serverA serverB

        Assert.That(deserializedServers.Length, Is.EqualTo 1)
        Assert.That(mergedServers.Length, Is.EqualTo 1)
        match deserializedServers.[0].CommunicationHistory,mergedServers.[0].CommunicationHistory with
        | Some (dHistory, dLastComm), Some (mHistory, mLastComm) ->
            match dHistory.Status,mHistory.Status with
            | Success,Success ->
                Assert.That(dLastComm, Is.EqualTo dummy_now)
                Assert.That(mLastComm, Is.EqualTo dummy_now)
            | _ -> Assert.Fail "both deserialized and merged should have status since both servers inserted had it #1"
        | _ ->
            Assert.Fail "both deserialized and merged should have some history since no server stored had None on it #2"

    [<Test>]
    member self.``when removing duplicate servers, the ones with history and most up to date, stay (IV)``() =
        let olderDate = dummy_now - TimeSpan.FromDays 1.0
        let sameRandomHostname = "xfoihror3uo3wmio"
        let serverA =
            {
                ServerInfo =
                    {
                        NetworkPath = sameRandomHostname
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = CreateFaultyHistoryInfo dummy_now
            }
        let serverB =
            {
                ServerInfo =
                    {
                        NetworkPath = sameRandomHostname
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = CreateHistoryInfoWithLsc olderDate
            }
        let deserializedServers = self.SerializeAndDeserialize serverA serverB
        let mergedServers = self.Merge serverA serverB

        Assert.That(deserializedServers.Length, Is.EqualTo 1)
        Assert.That(mergedServers.Length, Is.EqualTo 1)
        match deserializedServers.[0].CommunicationHistory,mergedServers.[0].CommunicationHistory with
        | Some (dHistory, dLastComm), Some (mHistory, mLastComm) ->
            match dHistory.Status,mHistory.Status with
            | Fault _,Fault _ ->
                Assert.That(dLastComm, Is.EqualTo dummy_now)
                Assert.That(mLastComm, Is.EqualTo dummy_now)
            | _ -> Assert.Fail "both deserialized and merged should have Fault"
        | _ ->
            Assert.Fail "both deserialized and merged should have some history since no server stored had None on it #2"

    [<Test>]
    member __.``blacklisted servers are removed``() =
        let serverA =
            {
                ServerInfo =
                    {
                        NetworkPath = "foo"
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = None
            }
        let serverB =
            {
                ServerInfo =
                    {
                        NetworkPath = "some.serverwithablacklistedname.blockscout.yes"
                        ConnectionType = some_connection_type_irrelevant_for_this_test
                    }
                CommunicationHistory = None
            }

        let servers = Map.empty.Add
                                (dummy_currency_because_irrelevant_for_this_test, seq { yield serverA; yield serverB })
        let serverDetails = ServerRegistry.Serialize servers
        let deserializedServers =
            ((ServerRegistry.Deserialize serverDetails).TryFind dummy_currency_because_irrelevant_for_this_test).Value
                |> List.ofSeq

        Assert.That(deserializedServers.Length, Is.EqualTo 1)

