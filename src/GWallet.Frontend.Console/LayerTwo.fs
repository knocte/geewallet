namespace GWallet.Frontend.Console

open System
open System.Net
open System.Linq

open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.UtxoCoin.Lightning
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks

module LayerTwo =

    let rec AskUtxoAccount (currency: Option<Currency>): IAccount =
        let account = UserInteraction.AskAccount()
        let validCurrencyRes =
            match currency with
            | None ->
                let lightningCurrencies = UtxoCoin.Lightning.Settings.Currencies
                if lightningCurrencies.Contains account.Currency then
                    Ok ()
                else
                    Error (
                        sprintf
                            "The only currencies supported for Lightning are %A, please select another account"
                            (String.Join("&", lightningCurrencies))
                    )
            | Some currency ->
                if currency = account.Currency then
                    Ok ()
                else 
                    Error "Currencies do not match"
        match validCurrencyRes with
        | Error msg ->
            Console.WriteLine msg
            AskUtxoAccount currency
        | Ok () ->
            match account with
            | :? UtxoCoin.NormalUtxoAccount as account -> account :> IAccount
            | :? UtxoCoin.ReadOnlyUtxoAccount as account -> account :> IAccount
            | :? UtxoCoin.ArchivedUtxoAccount ->
                Console.WriteLine "This account has been archived. You must select an active account"
                AskUtxoAccount currency
            | _ ->
                Console.WriteLine "Invalid account for use with Lightning"
                AskUtxoAccount currency


    let rec AskLightningAccount (currency: Option<Currency>): UtxoCoin.NormalUtxoAccount =
        match AskUtxoAccount currency with
        | :? UtxoCoin.NormalUtxoAccount as utxoAccount -> utxoAccount
        | :? UtxoCoin.ReadOnlyUtxoAccount ->
            Console.WriteLine "Read-only accounts cannot be used in lightning"
            AskLightningAccount currency
        | _ ->
            Console.WriteLine "Invalid account for use with Lightning"
            AskLightningAccount currency

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

    let ForceCloseChannel
        (node: Node)
        (currency: Currency)
        (channelId: ChannelIdentifier)
        : Async<unit> =
        async {
            let! forceCloseTxIdResult = node.ForceCloseChannel channelId
            Console.WriteLine (sprintf "Channel %s force closed" (ChannelId.ToString channelId))
            match forceCloseTxIdResult with
            | Ok forceCloseTxId ->
                let txUri = BlockExplorer.GetTransaction currency forceCloseTxId
                Console.WriteLine (
                    sprintf
                        "Funds recovered in this transaction:%s%s"
                        Environment.NewLine
                        (txUri.ToString ())
                )
            | Error ClosingBalanceBelowDustLimit ->
                Console.WriteLine "Closing balance of channel was too small (below the \"dust\" limit) so no funds were recovered."
        }

    let MaybeForceCloseChannel
        (node: Node)
        (currency: Currency)
        (channelId: ChannelIdentifier)
        (error: IErrorMsg)
        : Async<unit> =
            async {
                if error.ChannelBreakdown then
                    return! ForceCloseChannel node currency channelId
            }

    let OpenChannelFromReadOnlyAccount (fundingAccount: ReadOnlyUtxoAccount)
                                       (owningAccount: NormalUtxoAccount)
                                           : Async<unit> =
        async {
            let currency = (owningAccount :> IAccount).Currency
            let channelStore = ChannelStore owningAccount

            match UserInteraction.AskAmount fundingAccount with
            | None -> return ()
            | Some channelCapacity ->
                match AskChannelCounterpartyConnectionDetails currency with
                | None -> return ()
                | Some nodeEndPoint ->
                    let! metadataOpt = async {
                        try
                            let! metadata =
                                UtxoCoin.Lightning.ChannelManager.EstimateChannelOpeningFee
                                    (fundingAccount :> IAccount)
                                    channelCapacity
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
                            Console.WriteLine
                                "To proceed, you must enter the password for your online account"
                            let password = UserInteraction.AskPassword false
                            let nodeClient = Lightning.Connection.StartClient channelStore password
                            let! pendingChannelRes =
                                Lightning.Network.OpenChannel
                                    nodeClient
                                    nodeEndPoint
                                    channelCapacity
                            match pendingChannelRes with
                            | Error nodeOpenChannelError ->
                                Console.WriteLine (sprintf "Error opening channel: %s" nodeOpenChannelError.Message)
                            | Ok pendingChannel ->
                                let minimumDepth = (pendingChannel :> IChannelToBeOpened).ConfirmationsRequired
                                Console.WriteLine(
                                    sprintf
                                        "Opening a channel with this party will require %i confirmations (~%i minutes)"
                                        minimumDepth
                                        (minimumDepth * currency.BlockTimeInMinutes())
                                )
                                let acceptMinimumDepth = UserInteraction.AskYesNo "Do you accept?"
                                if acceptMinimumDepth then
                                    Console.WriteLine
                                        "This channel is being funded by an offline wallet."
                                    Console.WriteLine
                                        "Introduce a file name to save the unsigned funding transaction: "
                                    let filePath = Console.ReadLine()
                                    let proposal = {
                                        OriginAddress = (fundingAccount :> IAccount).PublicAddress
                                        Amount = pendingChannel.TransferAmount
                                        DestinationAddress = pendingChannel.FundingDestinationString()
                                    }
                                    Account.SaveUnsignedTransaction proposal metadata filePath
                                    Console.WriteLine "Transaction saved. Now copy it to the device with the private key for signing."
                                    let fileToReadFrom =
                                        UserInteraction.AskFileNameToLoad
                                            "Introduce a file name to load the signed funding transaction: "
                                    let signedTransaction =
                                        Account.LoadSignedTransactionFromFile fileToReadFrom.FullName
                                    let transactionDetails = GWallet.Backend.Account.GetSignedTransactionDetails signedTransaction
                                    Presentation.ShowTransactionData
                                        transactionDetails
                                        signedTransaction.TransactionInfo.Metadata
                                    if UserInteraction.AskYesNo "Do you accept?" then
                                        let! acceptRes =
                                            pendingChannel.AcceptWithFundingTx
                                                signedTransaction.RawTransaction
                                        match acceptRes with
                                        | Error fundChannelError ->
                                            Console.WriteLine(sprintf "Error funding channel: %s" fundChannelError.Message)
                                        | Ok (_channelId, txId) ->
                                            let uri = BlockExplorer.GetTransaction currency (TxId.ToString txId)
                                            Console.WriteLine(sprintf "A funding transaction was broadcast: %A" uri)

                    UserInteraction.PressAnyKeyToContinue()
        }

    let OpenChannelFromNormalAccount (account: NormalUtxoAccount): Async<unit> = async {
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
                        let! metadata = UtxoCoin.Lightning.ChannelManager.EstimateChannelOpeningFee (account :> IAccount) channelCapacity
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
                        match pendingChannelRes with
                        | Error nodeOpenChannelError ->
                            Console.WriteLine (sprintf "Error opening channel: %s" nodeOpenChannelError.Message)
                        | Ok pendingChannel ->
                            let minimumDepth = (pendingChannel :> IChannelToBeOpened).ConfirmationsRequired
                            Console.WriteLine(
                                sprintf
                                    "Opening a channel with this party will require %i confirmations (~%i minutes)"
                                    minimumDepth
                                    (minimumDepth * currency.BlockTimeInMinutes())
                            )
                            let acceptMinimumDepth = UserInteraction.AskYesNo "Do you accept?"
                            if acceptMinimumDepth then
                                let! acceptRes = pendingChannel.Accept metadata password
                                match acceptRes with
                                | Error fundChannelError ->
                                    Console.WriteLine(sprintf "Error funding channel: %s" fundChannelError.Message)
                                | Ok (_channelId, txId) ->
                                    let uri = BlockExplorer.GetTransaction currency (TxId.ToString txId)
                                    Console.WriteLine(sprintf "A funding transaction was broadcast: %A" uri)
                UserInteraction.PressAnyKeyToContinue()
    }

    let OpenChannel(): Async<unit> =
        match AskUtxoAccount None with
        | :? UtxoCoin.NormalUtxoAccount as account ->
             OpenChannelFromNormalAccount account
        | :? UtxoCoin.ReadOnlyUtxoAccount as fundingAccount ->
            Console.WriteLine "You've selected a read-only account to open a channel from"
            Console.WriteLine "Select the normal account which take control of the channel funds"
            let owningAccount =
                AskLightningAccount (Some (fundingAccount :> IAccount).Currency)
            OpenChannelFromReadOnlyAccount fundingAccount owningAccount
        | _ -> failwith "unreachable"

    let CloseChannel(): Async<unit> =
        async {
            let account = AskLightningAccount None
            let channelStore = ChannelStore account
            let channelIdOpt = AskChannelId channelStore None
            match channelIdOpt with
            | None -> return ()
            | Some channelId ->
                let currency = (account :> IAccount).Currency
                let channelInfo = channelStore.ChannelInfo channelId
                if channelInfo.IsFunder then
                    let password = UserInteraction.AskPassword false
                    let nodeClient = Lightning.Connection.StartClient channelStore password
                    let! closeRes = Lightning.Network.CloseChannel nodeClient channelId
                    match closeRes with
                    | Error closeError ->
                        Console.WriteLine(sprintf "Error closing channel: %s" (closeError :> IErrorMsg).Message)
                        if (closeError :> IErrorMsg).ChannelBreakdown then
                            return! ForceCloseChannel (Node.Client nodeClient) currency channelId
                        else
                            match closeError with
                            | NodeInitiateCloseChannelError.Reconnect _error ->
                                Console.WriteLine "Fundee node seems to be unreachable over the network, so can't close the channel cooperatively at the moment."
                                Console.WriteLine "You might want to wait and try again later, or force-close the channel at a last resort."
                                if UserInteraction.AskYesNo "Do you want to force-close the channel now?" then
                                    return! ForceCloseChannel (Node.Client nodeClient) currency channelId
                            | _ -> ()
                    | Ok () ->
                        Console.WriteLine "Channel closed."
                    UserInteraction.PressAnyKeyToContinue()
                    return ()
                else
                    Console.WriteLine "You are the fundee of this channel so, for it to be closed cooperatively:"
                    Console.WriteLine "1. Go back to main menu."
                    Console.WriteLine (
                        SPrintF1
                            "2. Choose option to '%s'"
                            (Presentation.ConvertPascalCaseToSentence (Operations.ReceiveLightningEvent.ToString()))
                    )
                    Console.WriteLine "3. Tell the funder to close the channel"
                    Console.WriteLine "But if you can't tell the funder, the last resort is to force-close the channel."
                    if UserInteraction.AskYesNo "Do you want to force-close the channel now?" then
                        let password = UserInteraction.AskPassword false
                        let nodeClient = Lightning.Connection.StartClient channelStore password
                        return! ForceCloseChannel (Node.Client nodeClient) currency channelId
        }


    let AcceptChannel(): Async<unit> =
        async {
            let account = AskLightningAccount None
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
            let account = AskLightningAccount None
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
                        let currency = (account :> IAccount).Currency
                        Console.WriteLine(sprintf "Error sending monohop payment: %s" nodeSendMonoHopPaymentError.Message)
                        do! MaybeForceCloseChannel (Node.Client nodeClient) currency channelId nodeSendMonoHopPaymentError
                    | Ok () ->
                        Console.WriteLine "Payment sent."
                    UserInteraction.PressAnyKeyToContinue()
        }

    let ReceiveLightningEvent(): Async<unit> =
        async {
            let account = AskLightningAccount None
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
                    let currency = (account :> IAccount).Currency
                    Console.WriteLine(sprintf "Error receiving lightning event: %s" nodeReceiveLightningEventError.Message)
                    do! MaybeForceCloseChannel (Node.Server nodeServer) currency channelId nodeReceiveLightningEventError
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
                    (currency)
                        : Async<seq<string>> =
        let channelId = channelInfo.ChannelId

        let lockChannelInternal (node: Node) (subLockFundingAsync: Async<Result<_, IErrorMsg>>): Async<seq<string>> =
            async {
                let! lockFundingRes = subLockFundingAsync
                match lockFundingRes with
                | Error lockFundingError ->
                    Console.WriteLine(sprintf "Error reestablishing channel: %s" lockFundingError.Message)
                    do! MaybeForceCloseChannel node currency channelId lockFundingError
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
                                      (currency)
                                          : Async<Async<seq<string>>> =
        async {
            let! remainingConfirmations = fundingBroadcastButNotLockedData.GetRemainingConfirmations()
            if remainingConfirmations = 0u then
                return LockChannel channelStore channelInfo currency
            else
                return async {
                    return seq {
                        yield! UserInteraction.DisplayLightningChannelStatus channelInfo
                        yield sprintf "        waiting for %i more confirmations" remainingConfirmations
                    }
                }
        }

    let UpdateFeeIfNecessary (channelStore: ChannelStore)
                             (channelInfo: ChannelInfo)
                                 : Async<unit> = async {
        let channelId = channelInfo.ChannelId
        let! feeUpdateRequired = channelStore.FeeUpdateRequired channelId
        match feeUpdateRequired with
        | None -> ()
        | Some feeRate ->
            Console.WriteLine(sprintf "Fee update needed for channel %s" (ChannelId.ToString channelId))
            Console.WriteLine "Ensure the fundee is ready to accept a connection to update the fee"
            Console.WriteLine "0. Cancel (at your own risk)"
            Console.WriteLine "1. Continue"

            let rec readInput() =
                let optIntroduced = Console.ReadLine()
                match Int32.TryParse(optIntroduced) with
                | false, _ -> readInput()
                | true, optionParsed ->
                    match optionParsed with
                    | 0 -> None
                    | 1 -> Some ()
                    | _ -> readInput()

            match readInput() with
            | Some () ->
                let password = UserInteraction.AskPassword false
                let nodeClient = Lightning.Connection.StartClient channelStore password
                let! updateFeeRes = (Node.Client nodeClient).UpdateFee channelId feeRate
                match updateFeeRes with
                | Error updateFeeError ->
                    Console.WriteLine(sprintf "Error updating fee: %s" updateFeeError.Message)
                | Ok () ->
                    Console.WriteLine(sprintf "Fee updated for channel %s" (ChannelId.ToString channelId))
            | None -> ()
    }

    let ClaimFundsIfTimelockExpired
        (channelStore: ChannelStore)
        (channelInfo: ChannelInfo)
        (locallyForceClosedData: LocallyForceClosedData)
        : Async<seq<string>> =
        async {
            let! remainingConfirmations = locallyForceClosedData.GetRemainingConfirmations()
            if remainingConfirmations = 0us then
                let! txId =
                    UtxoCoin.Account.BroadcastRawTransaction
                        locallyForceClosedData.Currency
                        locallyForceClosedData.SpendingTransactionString
                channelStore.DeleteChannel channelInfo.ChannelId
                return seq {
                    yield! UserInteraction.DisplayLightningChannelStatus channelInfo
                    yield sprintf "        channel force-closed"
                    yield sprintf "        funds have been recovered and returned to the wallet"
                    yield sprintf "        txid of recovery transaction is %s" txId
                }
            else
                return seq {
                    yield! UserInteraction.DisplayLightningChannelStatus channelInfo
                    yield sprintf "        channel force-closed"
                    yield sprintf
                        "        waiting for %i more confirmations before funds are recovered"
                        remainingConfirmations
                }
        }

    let FindRemoteForceClose
        (channelStore: ChannelStore)
        (channelInfo: ChannelInfo)
        : Async<Option<ForceCloseTx * Option<uint32>>> =
            async {
                let! closingTx =
                    channelStore.CheckForClosingTx channelInfo.ChannelId

                match closingTx with
                | Some (ClosingTx.ForceClose commitmentTx, closingTxDepth) ->
                    return Some (commitmentTx, closingTxDepth)
                | _ ->
                    return None
            }

    let ClaimFundsOnForceClose
        (channelStore: ChannelStore)
        (channelInfo: ChannelInfo)
        (closingTx: ForceCloseTx)
        (closingTxHeightOpt: Option<uint32>)
        : Async<seq<string>> =
        async {
            Console.WriteLine(sprintf "Channel %s has been force-closed by the counterparty.")
            Console.WriteLine "Account must be unlocked to recover funds."
            let password = UserInteraction.AskPassword false
            let nodeClient = Lightning.Connection.StartClient channelStore password
            let! recoveryTxStringResult =
                (Node.Client nodeClient).CreateRecoveryTxForRemoteForceClose
                    channelInfo.ChannelId
                    closingTx
                    // only use CPFP if closing transaction has not been confirmed yet
                    closingTxHeightOpt.IsNone
            match recoveryTxStringResult with
            | Ok recoveryTxString ->
                let! txIdString =
                    UtxoCoin.Account.BroadcastRawTransaction
                        channelStore.Currency
                        recoveryTxString
                channelStore.DeleteChannel channelInfo.ChannelId
                let txUri = BlockExplorer.GetTransaction (channelStore.Account :> IAccount).Currency txIdString
                return seq {
                    yield! UserInteraction.DisplayLightningChannelStatus channelInfo
                    yield "        channel closed by counterparty"
                    yield "        funds have been returned to wallet"
                    yield sprintf "        recovery transaction is: %s" (txUri.ToString())
                }
            | Error ClosingBalanceBelowDustLimit ->
                channelStore.DeleteChannel channelInfo.ChannelId
                return seq {
                    yield! UserInteraction.DisplayLightningChannelStatus channelInfo
                    yield "        channel closed by counterparty"
                    yield "        Local channel balance was too small (below the \"dust\" limit) so no funds were recovered."
                }

        }



    let GetChannelStatuses (accounts: seq<IAccount>): seq<Async<Async<seq<string>>>> =
        seq {
            let normalUtxoAccounts = accounts.OfType<UtxoCoin.NormalUtxoAccount>()
            for account in normalUtxoAccounts do
                let channelStore = ChannelStore account
                let currency = (account:> IAccount).Currency
                let channelIds =
                    channelStore.ListChannelIds()

                let isActive = 
                    fun (channelId: ChannelIdentifier) ->
                        let channelInfo = channelStore.ChannelInfo channelId
                        channelInfo.Status = ChannelStatus.Active
                let activeChannelCount = 
                    channelIds 
                    |> Seq.where isActive
                    |> Seq.length

                yield async {
                    return async {
                        return seq {
                            yield sprintf "%A Lightning Status (%i active channels)" currency activeChannelCount
                        }
                    }
                }
                for channelId in channelIds do
                    let channelInfo = channelStore.ChannelInfo channelId
                    match channelInfo.Status with
                    | ChannelStatus.FundingBroadcastButNotLocked fundingBroadcastButNotLockedData ->
                        let currency = (account :> IAccount).Currency
                        yield
                            LockChannelIfFundingConfirmed
                                channelStore
                                channelInfo
                                fundingBroadcastButNotLockedData
                                currency
                    | ChannelStatus.Active ->
                        yield
                            async {
                                let! remoteForceClosingTxOpt = FindRemoteForceClose channelStore channelInfo
                                match remoteForceClosingTxOpt with
                                | Some (closingTx, closingTxHeightOpt) ->
                                    return ClaimFundsOnForceClose channelStore channelInfo closingTx closingTxHeightOpt
                                | None ->
                                    return async {
                                        do! UpdateFeeIfNecessary channelStore channelInfo
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
                    | ChannelStatus.LocallyForceClosed locallyForceClosedData ->
                        yield async {
                            return
                                ClaimFundsIfTimelockExpired
                                    channelStore
                                    channelInfo
                                    locallyForceClosedData
                        }
                    | ChannelStatus.Closed ->
                        ()
        }
