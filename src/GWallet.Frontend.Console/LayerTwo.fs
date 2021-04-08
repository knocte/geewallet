﻿namespace GWallet.Frontend.Console

open System
open System.Net
open System.Linq

open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.UtxoCoin.Lightning
open GWallet.Backend.FSharpUtil

module LayerTwo =

    let rec AskLightningAccount(): UtxoCoin.NormalUtxoAccount =
        let account = UserInteraction.AskAccount()
        let lightningCurrency = UtxoCoin.Lightning.Settings.Currency
        if account.Currency <> lightningCurrency then
            Console.WriteLine (
                sprintf
                    "The only currency supported for Lightning is %A, please select a %A account"
                    lightningCurrency
                    lightningCurrency
            )
            AskLightningAccount ()
        else
            match account with
            | :? UtxoCoin.NormalUtxoAccount as btcAccount -> btcAccount
            | :? UtxoCoin.ReadOnlyUtxoAccount ->
                Console.WriteLine "Read-only accounts cannot be used in lightning"
                AskLightningAccount ()
            | :? UtxoCoin.ArchivedUtxoAccount ->
                Console.WriteLine "This account has been archived. You must select an active account"
                AskLightningAccount ()
            | _ ->
                Console.WriteLine "Invalid account for use with Lightning"
                AskLightningAccount ()

    let AskChannelCounterpartyConnectionDetails currency: Option<Lightning.NodeEndPoint> =
        let useQRString =
            UserInteraction.AskYesNo
                "Do you want to supply the channel counterparty connection string as used embedded in QR codes?"
        if useQRString then
            UserInteraction.Ask (Lightning.NodeEndPoint.Parse currency) "Channel counterparty QR connection string contents"
        else
            option {
                let! ipAddress =
                    UserInteraction.Ask IPAddress.Parse "Channel counterparty IP"
                let! port =
                    UserInteraction.Ask UInt16.Parse "Channel counterparty port"
                let! nodeId =
                    UserInteraction.Ask (PubKey.Parse currency) "Channel counterparty public key in hexadecimal notation"
                let ipEndPoint = IPEndPoint(ipAddress, int port)
                return Lightning.NodeEndPoint.FromParts nodeId ipEndPoint
            }

    let AskBindAddress(): IPEndPoint =
        let defaultIpAddress = "127.0.0.1"
        let defaultPort = 9735us
        let ipAddress =
            let ipAddressOpt =
                UserInteraction.Ask
                    IPAddress.Parse
                    (sprintf "IP address to bind to (leave blank for %s)" defaultIpAddress)
            match ipAddressOpt with
            | Some ipAddress -> ipAddress
            | None ->
                Console.WriteLine(sprintf "using default of %s" defaultIpAddress)
                IPAddress.Parse defaultIpAddress
        let port =
            let portOpt =
                UserInteraction.Ask
                    UInt16.Parse
                        (sprintf "Port to bind to (leave blank for %i)" defaultPort)
            match portOpt with
            | Some port -> port
            | None ->
                Console.WriteLine(sprintf "using default of %i" defaultPort)
                defaultPort
        IPEndPoint(ipAddress, int port)

    let rec AskChannelId (channelStore: ChannelStore)
                         (isFunderOpt: Option<bool>)
                             : Option<ChannelIdentifier> =
        let channelIds = seq {
            for channelId in channelStore.ListChannelIds() do
                let channelInfo = channelStore.ChannelInfo channelId
                match isFunderOpt with
                | None ->
                    yield channelId
                | Some isFunder ->
                    if channelInfo.IsFunder = isFunder then
                        yield channelId
        }

        Console.WriteLine "Available channels:"
        let rec listChannels (index: int) (channelIds: seq<ChannelIdentifier>) =
            if not <| Seq.isEmpty channelIds then
                let channelId = Seq.head channelIds
                Console.WriteLine(sprintf "%i: %s" index (ChannelId.ToString channelId))
                listChannels (index + 1) (Seq.tail channelIds)
        listChannels 0 channelIds

        let indexText = Console.ReadLine().Trim()
        if indexText = String.Empty then
            None
        else
            match Int32.TryParse indexText with
            | true, index when index < channelIds.Count() ->
                Some (channelIds.ElementAt index)
            | _ ->
                Console.WriteLine "Invalid option"
                AskChannelId channelStore isFunderOpt

    let MaybeForceCloseChannel
        (node: Node)
        (account)
        (channelId: ChannelIdentifier)
        (error: IErrorMsg)
        : Async<unit> =
            async {
                if error.ChannelBreakdown then
                    let channelStore = ChannelStore account
                    let! forceCloseTxId = node.ForceCloseChannel channelStore channelId
                    Console.WriteLine(sprintf "Channel %s force-closed. txid == %s" (ChannelId.ToString channelId) forceCloseTxId)
            }

    let OpenChannel(): Async<unit> =
        async {
            let account = AskLightningAccount ()
            let currency = (account :> IAccount).Currency
            let channelStore = ChannelStore account

            match UserInteraction.AskAmount account with
            | None -> return ()
            | Some channelCapacity ->
                match AskChannelCounterpartyConnectionDetails currency with
                | None -> return ()
                | Some nodeEndPoint ->
                    Infrastructure.LogDebug "Calling EstimateFee..."
                    let! metadataOpt = async {
                        try
                            let! metadata = UtxoCoin.Lightning.ChannelManager.EstimateChannelOpeningFee account channelCapacity
                            return Some metadata
                        with
                        | InsufficientBalanceForFee _ ->
                            Console.WriteLine
                                "Estimated fee is too high for the remaining balance, \
                                use a different account or a different amount."
                            return None
                    }
                    match metadataOpt with
                    | None -> return ()
                    | Some metadata ->
                        Presentation.ShowFeeAndSpendableBalance metadata channelCapacity

                        let acceptFeeRate = UserInteraction.AskYesNo "Do you accept?"
                        if acceptFeeRate then
                            let password = UserInteraction.AskPassword false
                            let nodeClient = Lightning.Connection.StartClient channelStore password
                            let! pendingChannelRes =
                                Lightning.Network.OpenChannel
                                    nodeClient
                                    nodeEndPoint
                                    channelCapacity
                                    metadata
                                    password
                            match pendingChannelRes with
                            | Error nodeOpenChannelError ->
                                Console.WriteLine (sprintf "Error opening channel: %s" nodeOpenChannelError.Message)
                            | Ok pendingChannel ->
                                let minimumDepth = (pendingChannel :> IChannelToBeOpened).ConfirmationsRequired
                                Console.WriteLine(
                                    sprintf
                                        "Opening a channel with this party will require %i confirmations (~%i minutes)"
                                        minimumDepth
                                        (minimumDepth * 10u)
                                )
                                let acceptMinimumDepth = UserInteraction.AskYesNo "Do you accept?"
                                if acceptMinimumDepth then
                                    let! txIdRes = pendingChannel.Accept()
                                    match txIdRes with
                                    | Error fundChannelError ->
                                        Console.WriteLine(sprintf "Error funding channel: %s" fundChannelError.Message)
                                    | Ok txId ->
                                        let uri = BlockExplorer.GetTransaction currency (TxId.ToString txId)
                                        Console.WriteLine(sprintf "A funding transaction was broadcast: %A" uri)
                    UserInteraction.PressAnyKeyToContinue()
        }

    let CloseChannel(): Async<unit> =
        async {
            let account = AskLightningAccount()
            let channelStore = ChannelStore account
            let channelIdOpt = AskChannelId channelStore None
            match channelIdOpt with
            | None -> return ()
            | Some channelId ->
                let password = UserInteraction.AskPassword false
                let nodeClient = Lightning.Connection.StartClient channelStore password
                let! closeRes = Lightning.Network.CloseChannel nodeClient channelId
                match closeRes with
                | Error closeError ->
                    Console.WriteLine(sprintf "Error closing channel: %s" (closeError :> IErrorMsg).Message)
                    do! MaybeForceCloseChannel (Node.Client nodeClient) account channelId closeError
                | Ok () ->
                    Console.WriteLine "Channel closed."
                UserInteraction.PressAnyKeyToContinue()
                return ()
        }


    let AcceptChannel(): Async<unit> =
        async {
            let account = AskLightningAccount ()
            let channelStore = ChannelStore account
            let bindAddress = AskBindAddress()
            let password = UserInteraction.AskPassword false
            use nodeServer = Lightning.Connection.StartServer channelStore password bindAddress
            let nodeEndPoint = Lightning.Network.EndPoint nodeServer
            Console.WriteLine(sprintf "This node, connect to it: %s" (nodeEndPoint.ToString()))
            let! acceptChannelRes = Lightning.Network.AcceptChannel nodeServer
            match acceptChannelRes with
            | Error nodeAcceptChannelError ->
                Console.WriteLine
                    (sprintf "Error accepting channel: %s" nodeAcceptChannelError.Message)
            | Ok (_, txId) ->
                Console.WriteLine (sprintf "Channel opened. Transaction ID: %s" (TxId.ToString txId))
                Console.WriteLine "Waiting for funding locked."
            UserInteraction.PressAnyKeyToContinue()
        }

    let SendPayment(): Async<unit> =
        async {
            let account = AskLightningAccount ()
            let channelStore = ChannelStore account
            let channelIdOpt = AskChannelId channelStore (Some true)
            match channelIdOpt with
            | None -> return ()
            | Some channelId ->
                let channelInfo = channelStore.ChannelInfo channelId
                let transferAmountOpt = UserInteraction.AskLightningAmount channelInfo
                match transferAmountOpt with
                | None -> ()
                | Some transferAmount ->
                    let password = UserInteraction.AskPassword false
                    let nodeClient = Lightning.Connection.StartClient channelStore password
                    let! paymentRes = Lightning.Network.SendMonoHopPayment nodeClient channelId transferAmount
                    match paymentRes with
                    | Error nodeSendMonoHopPaymentError ->
                        Console.WriteLine(sprintf "Error sending monohop payment: %s" nodeSendMonoHopPaymentError.Message)
                        do! MaybeForceCloseChannel (Node.Client nodeClient) account channelId nodeSendMonoHopPaymentError
                    | Ok () ->
                        Console.WriteLine "Payment sent."
                    UserInteraction.PressAnyKeyToContinue()
        }

    let ReceiveLightningEvent(): Async<unit> =
        async {
            let account = AskLightningAccount ()
            let channelStore = ChannelStore account
            let channelIdOpt = AskChannelId channelStore (Some false)
            match channelIdOpt with
            | None -> return ()
            | Some channelId ->
                let bindAddress = AskBindAddress()
                let password = UserInteraction.AskPassword false
                use nodeServer = Lightning.Connection.StartServer channelStore password bindAddress

                let! receiveLightningEventRes = Lightning.Network.ReceiveLightningEvent nodeServer channelId
                match receiveLightningEventRes with
                | Error nodeReceiveLightningEventError ->
                    Console.WriteLine(sprintf "Error receiving lightning event: %s" nodeReceiveLightningEventError.Message)
                    do! MaybeForceCloseChannel (Node.Server nodeServer) account channelId nodeReceiveLightningEventError
                | Ok msg ->
                    match msg with
                    | IncomingChannelEvent.MonoHopUnidirectionalPayment ->
                        Console.WriteLine "Payment received."
                    | IncomingChannelEvent.Shutdown ->
                        Console.WriteLine "Channel closed."
                UserInteraction.PressAnyKeyToContinue()
        }

    let LockChannel (channelStore: ChannelStore)
                    (channelInfo: ChannelInfo)
                        : Async<seq<string>> =
        let channelId = channelInfo.ChannelId

        let lockChannelInternal (node: Node) (subLockFundingAsync: Async<Result<_, IErrorMsg>>): Async<seq<string>> =
            async {
                let! lockFundingRes = subLockFundingAsync
                match lockFundingRes with
                | Error lockFundingError ->
                    Console.WriteLine(sprintf "Error reestablishing channel: %s" lockFundingError.Message)
                    do! MaybeForceCloseChannel node channelStore.Account channelId lockFundingError
                | Ok () ->
                    Console.WriteLine(sprintf "funding locked for channel %s" (ChannelId.ToString channelId))
                return seq {
                    yield! UserInteraction.DisplayLightningChannelStatus channelInfo
                    yield "        funding locked - channel is now active"
                }
            }

        Console.WriteLine(sprintf "Funding for channel %s confirmed" (ChannelId.ToString channelId))
        Console.WriteLine "In order to continue the funding for the channel needs to be locked"
        let lockFundingAsync =
            if channelInfo.IsFunder then
                Console.WriteLine
                    "Ensure the fundee is ready to accept a connection to lock the funding, \
                    then press any key to continue."
                Console.ReadKey true |> ignore
                let password = UserInteraction.AskPassword false
                async {
                    let nodeClient = Lightning.Connection.StartClient channelStore password
                    let sublockFundingAsync = Lightning.Network.ConnectLockChannelFunding nodeClient channelId
                    return! lockChannelInternal (Node.Client nodeClient) sublockFundingAsync
                }
            else
                let bindAddress =
                    Console.WriteLine "Listening for connection from peer"
                    AskBindAddress()
                let password = UserInteraction.AskPassword false
                async {
                    use nodeServer = Lightning.Connection.StartServer channelStore password bindAddress
                    let sublockFundingAsync = Lightning.Network.AcceptLockChannelFunding nodeServer channelId
                    return! lockChannelInternal (Node.Server nodeServer) sublockFundingAsync
                }
        lockFundingAsync

    let LockChannelIfFundingConfirmed (channelStore: ChannelStore)
                                      (channelInfo: ChannelInfo)
                                      (fundingBroadcastButNotLockedData: FundingBroadcastButNotLockedData)
                                          : Async<Async<seq<string>>> =
        async {
            let! remainingConfirmations = fundingBroadcastButNotLockedData.GetRemainingConfirmations()
            if remainingConfirmations = 0u then
                return LockChannel channelStore channelInfo
            else
                return async {
                    return seq {
                        yield! UserInteraction.DisplayLightningChannelStatus channelInfo
                        yield sprintf "        waiting for %i more confirmations" remainingConfirmations
                    }
                }
        }

    let GetChannelStatuses (accounts: seq<IAccount>): seq<Async<Async<seq<string>>>> =
        seq {
            let normalUtxoAccounts = accounts.OfType<UtxoCoin.NormalUtxoAccount>()
            for account in normalUtxoAccounts do
                let channelStore = ChannelStore account
                let channelIds = channelStore.ListChannelIds()
                yield async {
                    return async {
                        return seq {
                            yield sprintf "Lightning Status (%i channels)" (Seq.length channelIds)
                        }
                    }
                }
                for channelId in channelIds do
                    let channelInfo = channelStore.ChannelInfo channelId
                    match channelInfo.Status with
                    | ChannelStatus.FundingBroadcastButNotLocked fundingBroadcastButNotLockedData ->
                        yield
                            LockChannelIfFundingConfirmed channelStore channelInfo fundingBroadcastButNotLockedData
                    | ChannelStatus.Active ->
                        yield
                            async {
                                return async {
                                    return seq {
                                        yield! UserInteraction.DisplayLightningChannelStatus channelInfo
                                        yield "        channel is active"
                                    }
                                }
                            }
                    | ChannelStatus.Broken ->
                        yield
                            async {
                                return async {
                                    return seq {
                                        yield! UserInteraction.DisplayLightningChannelStatus channelInfo
                                        yield "        channel is in an abnormal state"
                                    }
                                }
                            }
                    | ChannelStatus.Closing ->
                        yield
                            async {
                                return async {
                                    let! isClosed = UtxoCoin.Lightning.Network.CheckClosingFinished channelInfo
                                    let showTheChannel = not isClosed
                                    if showTheChannel then
                                        return seq {
                                            yield! UserInteraction.DisplayLightningChannelStatus channelInfo
                                            yield "        channel is in being closed"
                                        }
                                    else
                                        return Seq.empty
                                }
                            }
                    | ChannelStatus.LocallyForceClosed _
                    | ChannelStatus.Closed ->
                        ()
        }
