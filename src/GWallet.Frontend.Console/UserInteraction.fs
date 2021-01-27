namespace GWallet.Frontend.Console

open System
open System.IO
open System.Linq
open System.Globalization

open GWallet.Backend

type internal Operations =
    | Exit               = 0
    | Refresh            = 1
    | CreateAccounts     = 2
    | SendPayment        = 3
    | AddReadonlyAccounts= 4
    | SignOffPayment     = 5
    | BroadcastPayment   = 6
    | ArchiveAccount     = 7
    | PairToWatchWallet  = 8
    | Options            = 9

type WhichAccount =
    All of seq<IAccount> | MatchingWith of IAccount

module UserInteraction =

    let PressAnyKeyToContinue() =
        Console.WriteLine ()
        Console.Write "Press any key to continue..."
        Console.ReadKey true
        |> ignore<ConsoleKeyInfo>
        Console.WriteLine ()

    // taken from InfraLib
    let ConsoleReadPasswordLine() =
        // taken from http://stackoverflow.com/questions/3404421/password-masking-console-application
        let rec ConsoleReadPasswordLineInternal(pwd: string) =
            let key = Console.ReadKey(true)

            if key.Key = ConsoleKey.Enter then
                Console.WriteLine()
                pwd
            else

                let newPwd =
                    if key.Key = ConsoleKey.Backspace && pwd.Length > 0 then
                        Console.Write("\b \b")
                        pwd.Substring(0, pwd.Length - 1)
                    else
                        Console.Write("*")
                        pwd + key.KeyChar.ToString()
                ConsoleReadPasswordLineInternal(newPwd)

        ConsoleReadPasswordLineInternal(String.Empty)

    exception NoOperationFound

    let rec FindMatchingOperation<'T> operationIntroduced (allOperations: List<'T*int>): 'T =
        match Int32.TryParse operationIntroduced with
        | false, _ -> raise NoOperationFound
        | true, operationParsed ->
            match allOperations with
            | [] -> raise NoOperationFound
            | (head,i)::tail ->
                if i = operationParsed then
                    head
                else
                    FindMatchingOperation operationIntroduced tail

    let internal OperationAvailable (operation: Operations) (numAccounts: int) =
        let noAccounts = numAccounts = 0
        match operation with
        | Operations.SendPayment
        | Operations.SignOffPayment
        | Operations.ArchiveAccount
        | Operations.PairToWatchWallet
        | Operations.Options
            ->
                not noAccounts
        | Operations.CreateAccounts -> noAccounts
        | _ -> true

    let rec internal AskFileNameToLoad (askText: string): FileInfo =
        Console.Write askText

        let fileName = Console.ReadLine()

        let file = FileInfo(fileName)
        if file.Exists then
            file
        else
            Presentation.Error "File not found, try again."
            AskFileNameToLoad askText

    let rec internal AskOperation (numAccounts: int): Operations =
        Console.WriteLine "Available operations:"

        // TODO: move these 2 lines below to FSharpUtil?
        let allOperations = (Enum.GetValues typeof<Operations>).Cast<Operations>() |> List.ofSeq

        let allOperationsAvailable =
            seq {
                for operation in allOperations do
                    if OperationAvailable operation numAccounts then
                        Console.WriteLine(sprintf "%d: %s"
                                              (int operation)
                                              (Presentation.ConvertPascalCaseToSentence (operation.ToString())))
                        yield operation, int operation
            } |> List.ofSeq
        Console.Write "Choose operation to perform: "
        let operationIntroduced = Console.ReadLine()
        try
            FindMatchingOperation operationIntroduced allOperationsAvailable
        with
        | :? NoOperationFound -> AskOperation numAccounts

    let rec private AskDob (repeat: bool): DateTime =
        let format = "dd/MM/yyyy"
        Console.Write(sprintf "Write your date of birth (format '%s'): " format)
        let dob = Console.ReadLine()
        match (DateTime.TryParseExact(dob, format, CultureInfo.InvariantCulture, DateTimeStyles.None)) with
        | false,_ ->
            Presentation.Error "Incorrect date or date format, please try again."
            AskDob repeat
        | true,parsedDateTime ->
            if repeat then
                Console.Write "Repeat it: "
                let dob2 = Console.ReadLine()
                if dob2 <> dob then
                    Presentation.Error "Dates don't match, please try again."
                    AskDob repeat
                else
                    parsedDateTime
            else
                parsedDateTime

    let rec private AskEmail (repeat: bool): string =
        if repeat then
            Console.Write "Write your e-mail address (that you'll never forget): "
        else
            Console.Write "Write your e-mail address: "

        let email = Console.ReadLine()
        if not repeat then
            email
        else
            Console.Write "Repeat it: "
            let email2 = Console.ReadLine()
            if email <> email2 then
                Presentation.Error "E-mail addresses are not the same, please try again."
                AskEmail repeat
            else
                email

    let rec private AskPassPhrase (repeat: bool): string =
        if repeat then
            Console.Write "Write a seed passphrase (for disaster recovery) for your new wallet: "
        else
            Console.Write "Write the seed passphrase: "

        let passphrase1 = ConsoleReadPasswordLine()
        if not repeat then
            passphrase1
        else
            Console.Write "Repeat the seed passphrase: "
            let passphrase2 = ConsoleReadPasswordLine()
            if passphrase1 <> passphrase2 then
                Presentation.Error "Passphrases are not the same, please try again."
                AskPassPhrase repeat
            else
                passphrase1

    let rec AskBrainSeed (repeat: bool): string*DateTime*string =
        Console.WriteLine()
        let passphrase = AskPassPhrase repeat
        let dob = AskDob repeat
        let email = AskEmail repeat
        passphrase,dob,email

    let rec AskPassword(repeat: bool): string =
        Console.WriteLine()
        if repeat then
            Console.Write "Write the password to unlock your account (when making payments) in this device: "
        else
            Console.Write "Write the password to unlock your account: "

        let password = ConsoleReadPasswordLine()
        if not repeat then
            password
        else
            Console.Write("Repeat the password: ")
            let password2 = ConsoleReadPasswordLine()
            if password <> password2 then
                Presentation.Error "Passwords are not the same, please try again."
                AskPassword(repeat)
            else
                password

    let private BalanceInUsdString balance maybeUsdValue =
        match maybeUsdValue with
        | NotFresh NotAvailable -> Presentation.ExchangeRateUnreachableMsg
        | Fresh usdValue ->
            sprintf "~ %s USD" (balance * usdValue |> Formatting.DecimalAmountRounding CurrencyType.Fiat)
        | NotFresh(Cached(usdValue,time)) ->
            sprintf "~ %s USD (last known rate as of %s)"
                (balance * usdValue |> Formatting.DecimalAmountRounding CurrencyType.Fiat)
                (time |> Formatting.ShowSaneDate)

    let private DisplayAccountStatusInner accountNumber
                                          (account: IAccount)
                                          (maybeBalance: MaybeCached<decimal>)
                                           maybeUsdValue
                                               : seq<string> =
        seq {
            let maybeReadOnly =
                match account with
                | :? ReadOnlyAccount -> "(READ-ONLY)"
                | _ -> String.Empty

            let accountInfo = sprintf "Account %d: %s%sCurrency=[%A] Address=[%s]"
                                    accountNumber maybeReadOnly Environment.NewLine
                                    account.Currency
                                    account.PublicAddress
            yield accountInfo

            match maybeBalance with
            | NotFresh NotAvailable ->
                yield "Unknown balance (Network unreachable... off-line?)"
            | NotFresh(Cached(balance,time)) ->
                let status = sprintf "Last known balance=[%s] (as of %s) %s %s"
                                    (balance |> Formatting.DecimalAmountRounding CurrencyType.Crypto)
                                    (time |> Formatting.ShowSaneDate)
                                    Environment.NewLine
                                    (BalanceInUsdString balance maybeUsdValue)
                yield status
            | Fresh balance ->
                let status = sprintf "Balance=[%s] %s"
                                    (balance |> Formatting.DecimalAmountRounding CurrencyType.Crypto)
                                    (BalanceInUsdString balance maybeUsdValue)
                yield status

            yield sprintf "History -> %s%s" ((BlockExplorer.GetTransactionHistory account).ToString())
                                            Environment.NewLine
        }

    let DisplayAccountStatus accountNumber (account: IAccount)
                                           (maybeBalance: MaybeCached<decimal>)
                                            maybeUsdValue
                                                : seq<string> =
        match account.Currency, maybeBalance with
        | Currency.SAI, Fresh 0m | Currency.DAI, Fresh 0m ->
            Seq.empty
        | _ ->
            DisplayAccountStatusInner accountNumber account maybeBalance maybeUsdValue

    let private GetAccountBalanceInner (account: IAccount): Async<IAccount*MaybeCached<decimal>*MaybeCached<decimal>> =
        async {
                // The console frontend cannot really take much advantage of the Fast|Analysis distinction here (as
                // opposed to the other frontends) because it doesn't have automatic balance refresh (it's this
                // operation the one that should only use Analysis mode). If we used Mode.Fast here, then the console
                // frontend would never re-discover slow/failing servers or even ones with no history
            let mode = ServerSelectionMode.Analysis

            let balanceJob = Account.GetShowableBalance account mode None
            let usdValueJob = FiatValueEstimation.UsdValue account.Currency
            let! balance,usdValue = FSharpUtil.AsyncExtensions.MixedParallel2 balanceJob usdValueJob
            return (account,balance,usdValue)
        }

    let private GetAccountBalance (account: IAccount): Async<MaybeCached<decimal>*MaybeCached<decimal>> =
        async {
            let! (_, balance, maybeUsdValue) = GetAccountBalanceInner account
            return (balance, maybeUsdValue)
        }

    let private GetAccountBalances (accounts: seq<IAccount>)
                                       : Async<array<IAccount*MaybeCached<decimal>*MaybeCached<decimal>>> =
        let accountAndBalancesToBeQueried = accounts |> Seq.map GetAccountBalanceInner
        Async.Parallel accountAndBalancesToBeQueried

    let DisplayAccountStatuses(whichAccount: WhichAccount): Async<seq<string>> =
        let rec displayAllAndSumBalance (accounts: seq<IAccount*MaybeCached<decimal>*MaybeCached<decimal>>)
                                         currentIndex
                                        (currentSumMap: Map<Currency,Option<decimal*MaybeCached<decimal>>>)
                                        : seq<string> * Map<Currency,Option<decimal*MaybeCached<decimal>>> =
            let account,maybeBalance,maybeUsdValue = accounts.ElementAt currentIndex
            let status = DisplayAccountStatus (currentIndex+1) account maybeBalance maybeUsdValue

            let balanceToSum: Option<decimal> =
                match maybeBalance with
                | Fresh balance -> Some balance
                | NotFresh (Cached(balance,_)) -> Some balance
                | _ -> None

            let newBalanceForCurrency =
                match balanceToSum with
                | None -> None
                | Some thisBalance ->
                    match Map.tryFind account.Currency currentSumMap with
                    | None ->
                        Some(thisBalance,maybeUsdValue)
                    | Some None ->
                        // there was a previous error, so we want to keep the total balance as N/A
                        None
                    | Some(Some(sumSoFar,_)) ->
                        Some(sumSoFar+thisBalance,maybeUsdValue)

            let maybeCleanedUpMapForReplacement =
                match Map.containsKey account.Currency currentSumMap with
                | false ->
                    currentSumMap
                | true ->
                    Map.remove account.Currency currentSumMap

            let newAcc = Map.add account.Currency newBalanceForCurrency maybeCleanedUpMapForReplacement

            if currentIndex < (accounts.Count() - 1) then
                let otherStatuses, acc = displayAllAndSumBalance accounts (currentIndex + 1) newAcc
                seq {
                    yield! status
                    yield! otherStatuses
                }, acc
            else
                status, newAcc

        let rec displayTotalAndSumFiatBalance (currenciesToBalances: Map<Currency,Option<decimal*MaybeCached<decimal>>>)
                                                  : Option<decimal> * seq<string> =
            let usdTotals =
                seq {
                    for KeyValue(currency, balance) in currenciesToBalances do
                        match balance with
                        | None -> ()
                        | Some(onlineBalance, maybeUsdValue) ->
                            match maybeUsdValue with
                            | NotFresh NotAvailable -> yield None, None
                            | Fresh usdValue | NotFresh(Cached(usdValue,_)) ->
                                let fiatValue = BalanceInUsdString onlineBalance maybeUsdValue
                                let cryptoValue = Formatting.DecimalAmountRounding CurrencyType.Crypto onlineBalance
                                let total = sprintf "Total %A: %s (%s)" currency cryptoValue fiatValue
                                yield Some(onlineBalance * usdValue), Some total
                } |> List.ofSeq
            let onlyValues = Seq.map fst usdTotals
            let totals: seq<string> = Seq.map snd usdTotals |> Seq.choose id
            if onlyValues.Any(fun maybeUsdTotal -> maybeUsdTotal.IsNone) then
                None, totals
            else
                Some(onlyValues.Sum(fun maybeUsdTotal -> maybeUsdTotal.Value)), totals

        match whichAccount with
        | WhichAccount.All(accounts) ->

            if accounts.Any() then
                async {
                    let! accountsWithBalances = GetAccountBalances accounts
                    let statuses, currencyTotals = displayAllAndSumBalance accountsWithBalances 0 Map.empty

                    let maybeTotalInUsd, totals = displayTotalAndSumFiatBalance currencyTotals
                    return
                        seq {
                            yield!
                                match maybeTotalInUsd with
                                | None -> statuses
                                | Some totalInUsd ->
                                    seq {
                                        yield! statuses
                                        yield! totals
                                        yield String.Empty // this ends up being simply an Environment.NewLine
                                        yield sprintf "Total estimated value in USD: %s"
                                                      (Formatting.DecimalAmountRounding CurrencyType.Fiat totalInUsd)
                                    }
                        }
                }
            else
                async {
                    return seq {
                        yield "No accounts have been created so far."
                    }
                }

        | MatchingWith(account) ->
            async {
                let allAccounts =  Account.GetAllActiveAccounts()
                let matchFilter = (fun (acc:IAccount) -> acc.PublicAddress = account.PublicAddress &&
                                                         acc.Currency = account.Currency &&
                                                         acc :? NormalAccount)
                let accountsMatching = allAccounts.Where matchFilter
                if accountsMatching.Count() <> 1 then
                    failwithf "account %s(%A) not found in config, or more than one with same public address?"
                              account.PublicAddress account.Currency
                let account = accountsMatching.Single()
                let! balance,maybeUsdValue = GetAccountBalance account
                return seq {
                    // this loop is just to find the number of the account
                    for i = 0 to allAccounts.Count() - 1 do
                        let iterAccount = allAccounts.ElementAt i
                        if matchFilter iterAccount then
                            yield! DisplayAccountStatus (i+1) iterAccount balance maybeUsdValue
                }
            }

    let rec AskYesNo (question: string): bool =
        Console.Write (sprintf "%s (Y/N): " question)
        let yesNoAnswer = Console.ReadLine().ToLowerInvariant()
        if yesNoAnswer = "y" then
            true
        elif yesNoAnswer = "n" then
            false
        else
            AskYesNo question

    let rec AskPublicAddress currency (askText: string): string =
        Console.Write askText
        let publicAddress = Console.ReadLine()
        let validatedAddress =
            try
                Account.ValidateAddress currency publicAddress
                    |> Async.RunSynchronously
                publicAddress
            with
            | InvalidDestinationAddress msg ->
                Presentation.Error msg
                AskPublicAddress currency askText
            | AddressMissingProperPrefix(possiblePrefixes) ->
                let possiblePrefixesStr = String.Join(", ", possiblePrefixes)
                Presentation.Error (sprintf "Address starts with the wrong prefix. Valid prefixes: %s"
                                        possiblePrefixesStr)
                AskPublicAddress currency askText

            | AddressWithInvalidLength lengthInfo ->
                match lengthInfo.Count() with
                | 1 ->
                    let lengthLimitViolated = lengthInfo.ElementAt 0
                    if publicAddress.Length <> lengthLimitViolated then
                        Presentation.Error
                            (sprintf "Address should have a length of %d characters, please try again."
                                lengthLimitViolated)
                    else
                        failwithf "Address introduced '%s' gave a length error with a limit that matches its length: %d=%d. Report this bug."
                                  publicAddress lengthLimitViolated publicAddress.Length
                | 2 ->
                    let minLength,maxLength = lengthInfo.ElementAt 0,lengthInfo.ElementAt 1
                    if publicAddress.Length < minLength then
                        Presentation.Error
                            (sprintf "Address should have a length not lower than %d characters, please try again."
                                minLength)
                    elif publicAddress.Length > maxLength then
                        Presentation.Error
                            (sprintf "Address should have a length not higher than %d characters, please try again."
                                maxLength)
                    else
                        Presentation.Error
                            (sprintf "Address should have a length of either %d or %d characters, please try again."
                                minLength maxLength)
                | _ ->
                    failwithf "AddressWithInvalidLength returned an invalid parameter length (%d). Report this bug."
                              (lengthInfo.Count())

                AskPublicAddress currency askText
            | AddressWithInvalidChecksum maybeAddressWithValidChecksum ->
                Console.Error.WriteLine "WARNING: the address provided didn't pass the checksum, are you sure you copied it properly?"
                Console.Error.WriteLine "(If you copied it by hand or somebody dictated it to you, you probably made a spelling mistake.)"
                match maybeAddressWithValidChecksum with
                | None ->
                    AskPublicAddress currency askText
                | Some addressWithValidChecksum ->
                    Console.Error.WriteLine "(If you used the clipboard, you're likely copying it from a service that doesn't have checksum validation.)"
                    let continueWithoutChecksum = AskYesNo "Continue with this address?"
                    if continueWithoutChecksum then
                        addressWithValidChecksum
                    else
                        AskPublicAddress currency askText
        validatedAddress

    type private AmountOption =
        | AllBalance
        | CertainCryptoAmount
        | ApproxEquivalentFiatAmount

    let rec private AskAmountOption(): AmountOption =
        Console.Write("Choose an option from the above: ")
        let optIntroduced = System.Console.ReadLine()
        match Int32.TryParse(optIntroduced) with
        | false, _ -> AskAmountOption()
        | true, optionParsed ->
            match optionParsed with
            | 1 -> AmountOption.CertainCryptoAmount
            | 2 -> AmountOption.ApproxEquivalentFiatAmount
            | 3 -> AmountOption.AllBalance
            | _ -> AskAmountOption()

    let rec AskParticularAmount() =
        Console.Write("Amount: ")
        let amount = Console.ReadLine()
        match Decimal.TryParse(amount) with
        | (false, _) ->
            Presentation.Error "Please enter a numeric amount."
            AskParticularAmount()
        | true, parsedAmount ->
            if not (parsedAmount > 0m) then
                Presentation.Error "Please enter a positive amount."
                AskParticularAmount()
            else
                parsedAmount

    let rec AskParticularUsdAmount currency usdValue (maybeTime:Option<DateTime>): Option<decimal> =
        let usdAmount = AskParticularAmount()
        let exchangeRateDateMsg =
            match maybeTime with
            | None -> String.Empty
            | Some time -> sprintf " (as of %s)" (Formatting.ShowSaneDate time)
        let exchangeMsg = sprintf "%s USD per %A%s" (usdValue.ToString())
                                                    currency
                                                    exchangeRateDateMsg
        let etherAmount = usdAmount / usdValue
        Console.WriteLine(sprintf "At an exchange rate of %s, %A amount would be:%s%s"
                              exchangeMsg currency
                              Environment.NewLine
                              (Formatting.DecimalAmountRounding CurrencyType.Crypto etherAmount))
        if AskYesNo "Do you accept?" then
            Some usdAmount
        else
            None

    let private AskParticularFiatAmountWithRate cryptoCurrency usdValue time: Option<decimal> =
        FSharpUtil.option {
            let! usdAmount = AskParticularUsdAmount cryptoCurrency usdValue time
            return usdAmount / usdValue
        }

    exception InsufficientBalance
    let rec internal AskAmount (account: IAccount): Option<TransferAmount> =
        let rec AskParticularAmountOption currentBalance (amountOption: AmountOption): Option<TransferAmount> =
            try
                match amountOption with
                | AmountOption.AllBalance ->
                    TransferAmount(currentBalance, currentBalance, account.Currency) |> Some
                | AmountOption.CertainCryptoAmount ->
                    let specificCryptoAmount = AskParticularAmount()
                    if specificCryptoAmount > currentBalance then
                        raise InsufficientBalance
                    TransferAmount(specificCryptoAmount, currentBalance, account.Currency) |> Some
                | AmountOption.ApproxEquivalentFiatAmount ->
                    match FiatValueEstimation.UsdValue account.Currency |> Async.RunSynchronously with
                    | NotFresh NotAvailable ->
                        Presentation.Error "USD exchange rate unreachable (offline?), please choose a different option."
                        AskAmount account
                    | Fresh usdValue ->
                        let maybeCryptoAmount = AskParticularFiatAmountWithRate account.Currency usdValue None
                        match maybeCryptoAmount with
                        | None -> None
                        | Some cryptoAmount ->
                            if cryptoAmount > currentBalance then
                                raise InsufficientBalance
                            TransferAmount(cryptoAmount, currentBalance, account.Currency) |> Some
                    | NotFresh(Cached(usdValue,time)) ->
                        let maybeCryptoAmount = AskParticularFiatAmountWithRate account.Currency usdValue (Some time)
                        match maybeCryptoAmount with
                        | None -> None
                        | Some cryptoAmount ->
                            if cryptoAmount > currentBalance then
                                raise InsufficientBalance
                            TransferAmount(cryptoAmount, currentBalance, account.Currency) |> Some
            with
            | :? InsufficientBalance ->
                Presentation.Error "Amount surpasses current balance, try again."
                AskParticularAmountOption currentBalance amountOption

        let showableBalance =
            Account.GetShowableBalance account ServerSelectionMode.Fast None
                |> Async.RunSynchronously

        match showableBalance with
        | NotFresh NotAvailable ->
            Presentation.Error "Balance not available if offline."
            None

        | Fresh balance | NotFresh(Cached(balance,_)) ->

            if not (balance > 0m) then
                // TODO: maybe we should check the balance before asking the destination address
                Presentation.Error "Account needs to have positive balance."
                None
            else
                Console.WriteLine "There are various options to specify the amount of your transaction:"
                Console.WriteLine(sprintf "1. Exact amount in %A" account.Currency)
                Console.WriteLine "2. Approximate amount in USD"
                Console.WriteLine(sprintf "3. All balance existing in the account (%g %A)"
                                          balance account.Currency)

                let amountOption = AskAmountOption()
                AskParticularAmountOption balance amountOption

    let AskFee account amount destination: Option<IBlockchainFeeInfo> =
        try
            let txMetadataWithFeeEstimation =
                Account.EstimateFee account amount destination |> Async.RunSynchronously
            Presentation.ShowFee amount.Currency txMetadataWithFeeEstimation
            let accept = AskYesNo "Do you accept?"
            if accept then
                Some txMetadataWithFeeEstimation
            else
                None
        with
        | InsufficientBalanceForFee maybeFeeValue ->
            // TODO: show fiat value in this error msg below?
            let errMsg =
                match maybeFeeValue with
                | Some feeValue ->
                    sprintf "Estimated fee is too high (%M) for the remaining balance, use a different account or a different amount."
                            feeValue
                | None ->
                    "Not enough balance to cover the estimated fee for this transaction plus the amount to be sent, use a different account or a different amount."
            Presentation.Error errMsg

            // TODO: instead of "press any key to continue...", it should ask amount again
            PressAnyKeyToContinue()

            None

    let rec AskAccount(): IAccount =
        let allAccounts = Account.GetAllActiveAccounts()
        Console.Write("Write the account number: ")
        let accountNumber = Console.ReadLine()
        match Int32.TryParse(accountNumber) with
        | false, _ -> AskAccount()
        | true, accountParsed ->
            let theAccountChosen =
                try
                    allAccounts.ElementAt(accountParsed - 1)
                with
                | _ -> AskAccount()
            theAccountChosen
