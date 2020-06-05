open System
open System.IO
open System.Linq

open FSharp.Core

open GWallet.Backend
open GWallet.Frontend.Console

// Alternative concise definition: http://www.fssnip.net/5Y/title/Repeat-until
let rec RetryOptionFunctionUntilSome (functionToRetry: unit -> Option<'T>): 'T =
    let returnedValue: Option<'T> = functionToRetry ()
    match returnedValue with
    | Some x -> x
    | None ->
        RetryOptionFunctionUntilSome functionToRetry

let OpenChannel() =
    let askChannelFee (account: UtxoCoin.NormalUtxoAccount)
                      (channelCapacity: TransferAmount)
                      (balance: decimal)
                      (channelCounterpartyIP: System.Net.IPEndPoint)
                      (channelCounterpartyPubKey: PublicKey)
                          : Async<Option<UtxoCoin.Lightning.ChannelCreationDetails>> =
        let currency = (account :> IAccount).Currency
        Infrastructure.LogDebug "Calling EstimateFee..."
        let metadata =
            try
                UtxoCoin.Lightning.ChannelManager.EstimateChannelOpeningFee
                     account channelCapacity
                     |> Async.RunSynchronously
            with
            | InsufficientBalanceForFee _ ->
                failwith "Estimated fee is too high for the remaining balance, \
                          use a different account or a different amount."

        let potentialChannel, channelEnvironment =
            UtxoCoin.Lightning.ChannelManager.GenerateNewPotentialChannelDetails account channelCounterpartyPubKey
        async {
            let! connectionBeforeAcceptChannelRes =
                UtxoCoin.Lightning.Network.ConnectAndHandshake channelEnvironment channelCounterpartyIP
            match connectionBeforeAcceptChannelRes with
            | Result.Error error ->
                Console.WriteLine error.Message
                return None
            | Result.Ok connectionBeforeAcceptChannel ->
                let passwordRef = ref "DotNetLightning shouldn't ask for password until later when user has
                                       confirmed the funding transaction fee. So this is a placeholder."
                let! maybeChannel =
                    UtxoCoin.Lightning.Network.OpenChannel
                        (account :> IAccount).Currency
                        potentialChannel
                        channelEnvironment
                        connectionBeforeAcceptChannel
                        channelCapacity
                        metadata
                        (fun _ -> !passwordRef)
                        balance
                match maybeChannel with
                | Result.Error error ->
                    Console.WriteLine error.Message
                    return None
                | Result.Ok outgoingUnfundedChannel ->
                    Presentation.ShowFee currency metadata
                    let confsReq = (outgoingUnfundedChannel :> UtxoCoin.Lightning.IChannelToBeOpened).ConfirmationsRequired
                    printfn
                        "Opening a channel with this party will require %i confirmations (~%i minutes)"
                        confsReq
                        (confsReq * 10u)
                    let accept = UserInteraction.AskYesNo "Do you accept?"

                    return
                        if accept then
                            let channelDetails: UtxoCoin.Lightning.ChannelCreationDetails =
                                {
                                    Client = connectionBeforeAcceptChannel.Client
                                    Password = passwordRef
                                    ChannelInfo = potentialChannel
                                    OutgoingUnfundedChannel = outgoingUnfundedChannel
                                }
                            channelDetails |> Some
                        else
                            connectionBeforeAcceptChannel.Client.Dispose()
                            None
        }

    let optionFromMaybeCachedBalance (balance: MaybeCached<decimal>): Option<decimal> =
        match balance with
        | NotFresh NotAvailable ->
            Presentation.Error "Can't open channel while offline."
            None
        | Fresh balance | NotFresh (Cached(balance, _)) ->
            Some balance

    let lnCurrency = UtxoCoin.Lightning.Settings.Currency
    let lnAccount = Account
                        .GetAllActiveAccounts()
                        .OfType<UtxoCoin.NormalUtxoAccount>()
                        .Single(fun account -> (account :> IAccount).Currency = lnCurrency)
    let balance = Account.GetShowableBalance lnAccount ServerSelectionMode.Fast None
                  |> Async.RunSynchronously
    let maybeChannelCreationDetails =
        FSharpUtil.option {
            let! balance = optionFromMaybeCachedBalance balance
            let! channelCapacity = UserInteraction.AskAmount lnAccount
            let! ipEndpoint, pubKey = UserInteraction.AskChannelCounterpartyConnectionDetails lnCurrency
            Infrastructure.LogDebug "Getting channel fee..."
            let! channelCreationDetails =
                askChannelFee
                    lnAccount
                    channelCapacity
                    balance
                    ipEndpoint
                    pubKey
                    |> Async.RunSynchronously
            return ipEndpoint, channelCreationDetails
        }
    match maybeChannelCreationDetails with
    | Some (ipEndpoint, details) ->
        Infrastructure.LogDebug "Opening channel..."
        let txIdRes =
            let attempt (): Option<Result<string, UtxoCoin.Lightning.Network.LNError>> =
                let password = UserInteraction.AskPassword false
                // Password is a reference, it is also inside details.Channel,
                // so while it looks unused, it is indeed used.
                details.Password := password
                try
                    let txIdRes =
                        UtxoCoin.Lightning.Network.ContinueFromAcceptChannelAndSave
                            lnAccount
                            ipEndpoint
                            details
                            |> Async.RunSynchronously
                    details.Client.Dispose()
                    Some txIdRes
                with
                | :? InvalidPassword ->
                    printfn "Invalid password, try again."
                    None
            RetryOptionFunctionUntilSome attempt
        match txIdRes with
        | Result.Error error ->
            Console.WriteLine error.Message
        | Result.Ok txId ->
            let uri = BlockExplorer.GetTransaction lnCurrency txId
            printfn "A funding transaction was broadcast: %A" uri
        UserInteraction.PressAnyKeyToContinue()
    | None ->
        // Error message printed already
        UserInteraction.PressAnyKeyToContinue()

let AcceptChannel() =
    let lightningAccount = Account
                               .GetAllActiveAccounts()
                               .OfType<UtxoCoin.NormalUtxoAccount>()
                               .Single(fun account ->
                                   (account :> IAccount).Currency = UtxoCoin.Lightning.Settings.Currency)
    let nodeUrl,job = UtxoCoin.Lightning.Network.AcceptTheirChannel lightningAccount
    Console.WriteLine (sprintf "This node, connect to it: %s" nodeUrl)
    match job |> Async.RunSynchronously with
    | Result.Error error ->
        Console.WriteLine error.Message
    | Result.Ok () ->
        ()

let rec TrySendAmount (account: NormalAccount) transactionMetadata destination amount =
    let password = UserInteraction.AskPassword false
    try
        let txIdUri =
            Account.SendPayment account transactionMetadata destination amount password
                |> Async.RunSynchronously
        Console.WriteLine(sprintf "Transaction successful:%s%s" Environment.NewLine (txIdUri.ToString()))
        UserInteraction.PressAnyKeyToContinue ()
    with
    | :? DestinationEqualToOrigin ->
        Presentation.Error "Transaction's origin cannot be the same as the destination."
        UserInteraction.PressAnyKeyToContinue()
    | :? InsufficientFunds ->
        Presentation.Error "Insufficient funds."
        UserInteraction.PressAnyKeyToContinue()
    | :? InvalidPassword ->
        Presentation.Error "Invalid password, try again."
        TrySendAmount account transactionMetadata destination amount

let rec TrySign account unsignedTrans =
    let password = UserInteraction.AskPassword false
    try
        Account.SignUnsignedTransaction account unsignedTrans password
    with
    // TODO: would this throw insufficient funds? test
    //| :? InsufficientFunds ->
    //    Presentation.Error "Insufficient funds."
    | :? InvalidPassword ->
        Presentation.Error "Invalid password, try again."
        TrySign account unsignedTrans

let BroadcastPayment() =
    let fileToReadFrom = UserInteraction.AskFileNameToLoad
                             "Introduce a file name to load the signed transaction: "
    let signedTransaction = Account.LoadSignedTransactionFromFile fileToReadFrom.FullName
    //TODO: check if nonce matches, if not, reject trans

    let transactionDetails = Account.GetSignedTransactionDetails signedTransaction
    Presentation.ShowTransactionData
        transactionDetails
        signedTransaction.TransactionInfo.Metadata

    if UserInteraction.AskYesNo "Do you accept?" then
        try
            let txIdUri =
                Account.BroadcastTransaction signedTransaction
                    |> Async.RunSynchronously
            Console.WriteLine(sprintf "Transaction successful:%s%s" Environment.NewLine (txIdUri.ToString()))
            UserInteraction.PressAnyKeyToContinue ()
        with
        | :? DestinationEqualToOrigin ->
            Presentation.Error "Transaction's origin cannot be the same as the destination."
            UserInteraction.PressAnyKeyToContinue()
        | :? InsufficientFunds ->
            Presentation.Error "Insufficient funds."
            UserInteraction.PressAnyKeyToContinue()

let SignOffPayment() =
    let fileToReadFrom = UserInteraction.AskFileNameToLoad
                             "Introduce a file name to load the unsigned transaction: "
    let unsignedTransaction = Account.LoadUnsignedTransactionFromFile fileToReadFrom.FullName

    let accountsWithSameAddress =
        Account.GetAllActiveAccounts().Where(fun acc -> acc.PublicAddress = unsignedTransaction.Proposal.OriginAddress)
    if not (accountsWithSameAddress.Any()) then
        Presentation.Error "The transaction doesn't correspond to any of the accounts in the wallet."
        UserInteraction.PressAnyKeyToContinue ()
    else
        let accounts =
            accountsWithSameAddress.Where(
                fun acc -> acc.Currency = unsignedTransaction.Proposal.Amount.Currency &&
                           acc :? NormalAccount)
        if not (accounts.Any()) then
            Presentation.Error(
                sprintf
                    "The transaction corresponds to an address of the accounts in this wallet, but it's a readonly account or it maps a different currency than %A."
                    unsignedTransaction.Proposal.Amount.Currency
            )
            UserInteraction.PressAnyKeyToContinue()
        else
            let account = accounts.First()
            if (accounts.Count() > 1) then
                failwith "More than one normal account matching address and currency? Please report this issue."

            match account with
            | :? ReadOnlyAccount ->
                failwith "Previous account filtering should have discarded readonly accounts already. Please report this issue"
            | :? NormalAccount as normalAccount ->
                Console.WriteLine ("Importing external data...")
                Caching.Instance.SaveSnapshot unsignedTransaction.Cache

                let lines = UserInteraction.DisplayAccountStatuses <| WhichAccount.MatchingWith account
                               |> Async.RunSynchronously
                Console.WriteLine ("Account to use when signing off this transaction:")
                Console.WriteLine ()
                for line in lines do
                    Console.WriteLine line
                Console.WriteLine()

                Presentation.ShowTransactionData
                    (unsignedTransaction.Proposal :> ITransactionDetails)
                    unsignedTransaction.Metadata

                if UserInteraction.AskYesNo "Do you accept?" then
                    let trans = TrySign normalAccount unsignedTransaction
                    Console.WriteLine("Transaction signed.")
                    Console.Write("Introduce a file name or path to save it: ")
                    let filePathToSaveTo = Console.ReadLine()
                    Account.SaveSignedTransaction trans filePathToSaveTo
                    Console.WriteLine("Transaction signed and saved successfully. Now copy it to the online device.")
                    UserInteraction.PressAnyKeyToContinue ()
            | _ ->
                failwith "Account type not supported. Please report this issue."

let SendPaymentOfSpecificAmount (account: IAccount)
                                (amount: TransferAmount)
                                (transactionMetadata: IBlockchainFeeInfo)
                                (destination: string) =
    match account with
    | :? ReadOnlyAccount ->
        Console.WriteLine("Cannot send payments from readonly accounts.")
        Console.Write("Introduce a file name to save the unsigned transaction: ")
        let filePath = Console.ReadLine()
        let proposal = {
            OriginAddress = account.PublicAddress;
            Amount = amount;
            DestinationAddress = destination;
        }
        Account.SaveUnsignedTransaction proposal transactionMetadata filePath
        Console.WriteLine("Transaction saved. Now copy it to the device with the private key.")
        UserInteraction.PressAnyKeyToContinue()
    | :? NormalAccount as normalAccount ->
        TrySendAmount normalAccount transactionMetadata destination amount
    | _ ->
        failwith ("Account type not recognized: " + account.GetType().FullName)

let SendPayment() =
    let account = UserInteraction.AskAccount()
    let destination = UserInteraction.AskPublicAddress account.Currency "Destination address: "
    let maybeAmount = UserInteraction.AskAmount account
    match maybeAmount with
    | None -> ()
    | Some(amount) ->
        let maybeFee = UserInteraction.AskFee account amount destination
        match maybeFee with
        | None -> ()
        | Some(fee) ->
            SendPaymentOfSpecificAmount account amount fee destination

let rec TryArchiveAccount account =
    let password = UserInteraction.AskPassword(false)
    try
        Account.Archive account password
        Console.WriteLine "Account archived."
        UserInteraction.PressAnyKeyToContinue ()
    with
    | :? InvalidPassword ->
        Presentation.Error "Invalid password, try again."
        TryArchiveAccount account

let rec AddReadOnlyAccounts() =
    Console.Write "JSON fragment from wallet to pair with: "
    let watchWalletInfoJson = Console.ReadLine().Trim()
    let watchWalletInfo = Marshalling.Deserialize watchWalletInfoJson
    Account.CreateReadOnlyAccounts watchWalletInfo

let ArchiveAccount() =
    let account = UserInteraction.AskAccount()
    match account with
    | :? ReadOnlyAccount as readOnlyAccount ->
        Console.WriteLine("Read-only accounts cannot be archived, but just removed entirely.")
        if not (UserInteraction.AskYesNo "Do you accept?") then
            ()
        else
            Account.Remove readOnlyAccount
            Console.WriteLine "Read-only account removed."
            UserInteraction.PressAnyKeyToContinue()
    | :? NormalAccount as normalAccount ->
        let balance =
            Account.GetShowableBalance account ServerSelectionMode.Fast None
                |> Async.RunSynchronously
        match balance with
        | NotFresh(NotAvailable) ->
            Presentation.Error "Removing accounts when offline is not supported."
            ()
        | Fresh(amount) | NotFresh(Cached(amount,_)) ->
            if (amount > 0m) then
                Presentation.Error "Please empty the account before removing it."
                UserInteraction.PressAnyKeyToContinue ()
            else
                Console.WriteLine ()
                Console.WriteLine "Please note: "
                Console.WriteLine "Just in case this account receives funds in the future by mistake, "
                Console.WriteLine "the operation of archiving an account doesn't entirely remove it."
                Console.WriteLine ()
                Console.WriteLine "You will be asked the password of it now so that its private key can remain unencrypted in the configuration folder, in order for you to be able to safely forget this password."
                Console.WriteLine "Then this account will be watched constantly and if new payments are detected, "
                Console.WriteLine "GWallet will prompt you to move them to a current account without the need of typing the old password."
                Console.WriteLine ()
                if not (UserInteraction.AskYesNo "Do you accept?") then
                    ()
                else
                    TryArchiveAccount normalAccount
    | _ ->
        failwithf "Account type not valid for archiving: %s. Please report this issue."
                  (account.GetType().FullName)

let PairToWatchWallet() =
    match Account.GetNormalAccountsPairingInfoForWatchWallet() with
    | None ->
        Presentation.Error
            "There needs to be both Ether-based accounts and Utxo-based accounts to be able to use this feature."
    | Some walletInfo ->
        Console.WriteLine ""
        Console.WriteLine "Copy/paste this JSON fragment in your watching wallet:"
        Console.WriteLine ""
        let json = Marshalling.SerializeOneLine walletInfo
        Console.WriteLine json
        Console.WriteLine ""

    UserInteraction.PressAnyKeyToContinue()

type private GenericWalletOption =
    | Cancel
    | TestPaymentPassword
    | TestSeedPassphrase
    | WipeWallet

let rec TestPaymentPassword () =
    let password = UserInteraction.AskPassword false
    let passwordChecksOnAllAccounts = Account.CheckValidPassword password |> Async.RunSynchronously
    if not (passwordChecksOnAllAccounts.All(fun x -> x = true)) then
        Console.WriteLine "Try again."
        TestPaymentPassword ()

let rec TestSeedPassphrase(): unit =
    let passphrase,dob,email = UserInteraction.AskBrainSeed false
    let check = Account.CheckValidSeed passphrase dob email |> Async.RunSynchronously
    if not check then
        Console.WriteLine "Try again."
        TestSeedPassphrase()

let WipeWallet() =
    Console.WriteLine "If you want to remove accounts, the recommended way is to archive them, not wipe the whole wallet."
    Console.Write "Are you ABSOLUTELY SURE about this? If yes, write 'YES' in uppercase: "
    let sure = Console.ReadLine ()
    if sure = "YES" then
        Account.WipeAll()
    else
        ()

let WalletOptions(): unit =
    let rec AskWalletOption(): GenericWalletOption =
        Console.WriteLine "0. Cancel, go back"
        Console.WriteLine "1. Check you still remember your payment password"
        Console.WriteLine "2. Check you still remember your seed passphrase"
        Console.WriteLine "3. Wipe your current wallet, in order to start from scratch"
        Console.Write "Choose an option from the ones above: "
        let optIntroduced = Console.ReadLine ()
        match UInt32.TryParse optIntroduced with
        | false, _ -> AskWalletOption()
        | true, optionParsed ->
            match optionParsed with
            | 0u -> GenericWalletOption.Cancel
            | 1u -> GenericWalletOption.TestPaymentPassword
            | 2u -> GenericWalletOption.TestSeedPassphrase
            | 3u -> GenericWalletOption.WipeWallet
            | _ -> AskWalletOption()

    let walletOption = AskWalletOption()
    match walletOption with
    | GenericWalletOption.TestPaymentPassword ->
        TestPaymentPassword()
        Console.WriteLine "Success!"
    | GenericWalletOption.TestSeedPassphrase ->
        TestSeedPassphrase()
        Console.WriteLine "Success!"
    | GenericWalletOption.WipeWallet ->
        WipeWallet()
    | _ -> ()

let rec PerformOperation (numActiveAccounts: uint32) (numHotAccounts: uint32) =
    match UserInteraction.AskOperation numActiveAccounts numHotAccounts with
    | Operations.Exit -> exit 0
    | Operations.CreateAccounts ->
        let bootstrapTask = Caching.Instance.BootstrapServerStatsFromTrustedSource() |> Async.StartAsTask
        let passphrase,dob,email = UserInteraction.AskBrainSeed true
        if null <> bootstrapTask.Exception then
            raise bootstrapTask.Exception
        let masterPrivateKeyTask =
            Account.GenerateMasterPrivateKey passphrase dob email
                |> Async.StartAsTask
        let password = UserInteraction.AskPassword true
        Async.RunSynchronously <| async {
            let! privateKeyBytes = Async.AwaitTask masterPrivateKeyTask
            return! Account.CreateAllAccounts privateKeyBytes password
        }
        Console.WriteLine("Accounts created")
        UserInteraction.PressAnyKeyToContinue()
    | Operations.Refresh -> ()
    | Operations.SendPayment ->
        SendPayment()
    | Operations.AddReadonlyAccounts ->
        AddReadOnlyAccounts()
            |> Async.RunSynchronously
    | Operations.SignOffPayment ->
        SignOffPayment()
    | Operations.BroadcastPayment ->
        BroadcastPayment()
    | Operations.ArchiveAccount ->
        ArchiveAccount()
    | Operations.PairToWatchWallet ->
        PairToWatchWallet()
    | Operations.Options ->
        WalletOptions()
    | Operations.OpenChannel ->
        OpenChannel()
    | Operations.AcceptChannel ->
        AcceptChannel()
    | _ -> failwith "Unreachable"

let rec GetAccountOfSameCurrency currency =
    let account = UserInteraction.AskAccount()
    if (account.Currency <> currency) then
        Presentation.Error (sprintf "The account selected doesn't match the currency %A" currency)
        GetAccountOfSameCurrency currency
    else
        account

let rec CheckArchivedAccountsAreEmpty(): bool =
    let archivedAccountsInNeedOfAction =
        Account.GetArchivedAccountsWithPositiveBalance None
            |> Async.RunSynchronously
    for archivedAccount,balance in archivedAccountsInNeedOfAction do
        let currency = (archivedAccount:>IAccount).Currency
        Console.WriteLine (sprintf "ALERT! An archived account has received funds:%sAddress: %s Balance: %s%A"
                               Environment.NewLine
                               (archivedAccount:>IAccount).PublicAddress
                               (balance.ToString())
                               currency)
        Console.WriteLine "Please indicate the account you would like to transfer the funds to."
        let account = GetAccountOfSameCurrency currency

        let allBalance = TransferAmount(balance, balance, account.Currency)
        let maybeFee = UserInteraction.AskFee archivedAccount allBalance account.PublicAddress
        match maybeFee with
        | None -> ()
        | Some(feeInfo) ->
            let txId =
                Account.SweepArchivedFunds archivedAccount balance account feeInfo
                    |> Async.RunSynchronously
            Console.WriteLine(sprintf "Transaction successful, its ID is:%s%s" Environment.NewLine txId)
            UserInteraction.PressAnyKeyToContinue ()

    not (archivedAccountsInNeedOfAction.Any())

let private NotReadyReasonToString (reason: UtxoCoin.Lightning.ChannelNotReadyReason): string =
    sprintf "%i out of %i confirmations" reason.CurrentConfirmations reason.NeededConfirmations

let private CheckChannelStatus (currency, channelFile: FileInfo, channelFileId: int): Async<seq<string>> =
    async {
        let! channelStatusRes = UtxoCoin.Lightning.Network.LoadChannelCheckingChannelMessage currency channelFile
        match channelStatusRes with
        | Result.Error error ->
            return seq {
                yield error.Message
            }
        | Result.Ok channelStatus ->
            return
                match channelStatus with
                | UtxoCoin.Lightning.ChannelStatus.UnusableChannelWithReason (txIdHex, notReadyReason) ->
                    let reasonString = NotReadyReasonToString notReadyReason
                    let msg =
                        sprintf
                            "Channel %i opening in progress (%s): %s%s"
                            channelFileId reasonString txIdHex Environment.NewLine
                    seq { yield msg }
                | UtxoCoin.Lightning.ChannelStatus.UsableChannel _ ->
                    seq { yield sprintf "Channel %i is open%s" channelFileId Environment.NewLine }
    }

let private CheckChannelStatuses(): Async<seq<string>> =
    async {
        let jobs = UtxoCoin.Lightning.ChannelManager.ListSavedChannels () |> Seq.map CheckChannelStatus
        let! statuses = Async.Parallel jobs
        return Seq.collect id statuses
    }

let rec ProgramMainLoop() =
    let activeAccounts = Account.GetAllActiveAccounts()
    let hotAccounts =
        activeAccounts.Where(
            fun acc ->
                match acc with
                | :? NormalAccount -> true
                | _ -> false
        )

    Console.WriteLine ()
    Console.WriteLine "*** STATUS ***"
    let statusAndChannelJob =
        seq {
            yield UserInteraction.DisplayAccountStatuses (WhichAccount.All activeAccounts)
            yield CheckChannelStatuses()
        } |> Async.Parallel
    let results = Async.RunSynchronously statusAndChannelJob
    let accountStatuses: seq<string> = results.[0]
    let channelStatuses: seq<string> = results.[1]
    let lines: seq<string> =
        seq {
            yield! accountStatuses
            yield Environment.NewLine
            yield! channelStatuses
        }
    Console.WriteLine (String.concat Environment.NewLine lines)
    Console.WriteLine ()

    if CheckArchivedAccountsAreEmpty() then
        PerformOperation (uint32 (activeAccounts.Count())) (uint32 (hotAccounts.Count()))
    ProgramMainLoop()


let NormalStartWithNoParameters () =

    Infrastructure.SetupExceptionHook ()

    let exitCode =
        try
            ProgramMainLoop ()
            0
        with
        | ex ->
            Infrastructure.LogOrReportCrash ex
            1

    exitCode


let UpdateServersFile () =
    ServerManager.UpdateServersFile()
    0

let UpdateServersStats () =
    ServerManager.UpdateServersStats()
    0

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 0 ->
        NormalStartWithNoParameters()
    | 1 when argv.[0] = "--update-servers-file" ->
        UpdateServersFile()
    | 1 when argv.[0] = "--update-servers-stats" ->
        UpdateServersStats()
    | _ ->
        failwith "Arguments not recognized"
