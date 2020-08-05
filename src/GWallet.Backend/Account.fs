namespace GWallet.Backend

open System
open System.Linq
open System.IO
open System.Threading.Tasks

open GWallet.Backend.FSharpUtil.UwpHacks

module Account =

    let private GetShowableBalanceInternal (account: IAccount)
                                           (mode: ServerSelectionMode)
                                           (cancelSourceOption: Option<CustomCancelSource>)
                                               : Async<Option<decimal>> =
        match account with
        | :? UtxoCoin.IUtxoAccount as utxoAccount ->
            if not (account.Currency.IsUtxo()) then
                failwith <| SPrintF1 "Currency %A not Utxo-type but account is? report this bug (balance)"
                                     account.Currency

            UtxoCoin.Account.GetShowableBalance utxoAccount mode cancelSourceOption
        | _ ->
            if not (account.Currency.IsEtherBased()) then
                failwith <| SPrintF1 "Currency %A not ether based and not UTXO either? not supported, report this bug (balance)"
                    account.Currency
            Ether.Account.GetShowableBalance account mode cancelSourceOption

    let GetShowableBalance (account: IAccount)
                           (mode: ServerSelectionMode)
                           (cancelSourceOption: Option<CustomCancelSource>)
                               : Async<MaybeCached<decimal>> =
        async {
            if Config.NoNetworkBalanceForDebuggingPurposes then
                return Fresh 1m
            else

            let! maybeBalance = GetShowableBalanceInternal account mode cancelSourceOption
            match maybeBalance with
            | None ->
                return NotFresh(Caching.Instance.RetrieveLastCompoundBalance account.PublicAddress account.Currency)
            | Some balance ->
                let compoundBalance,_ =
                    Caching.Instance.RetrieveAndUpdateLastCompoundBalance account.PublicAddress
                                                                          account.Currency
                                                                          balance
                return Fresh compoundBalance
        }

    let internal GetAccountFromFile accountFile (currency: Currency) kind: IAccount =
        if currency.IsUtxo() then
            UtxoCoin.Account.GetAccountFromFile accountFile currency kind
        elif currency.IsEtherBased() then
            Ether.Account.GetAccountFromFile accountFile currency kind
        else
            failwith <| SPrintF1 "Currency (%A) not supported for this API" currency

    let GetAllActiveAccounts(): seq<IAccount> =
        Config.PropagateEthAccountInfoToMissingTokensAccounts()

        let allCurrencies =
            if Config.BitcoinNet() = NBitcoin.Network.RegTest then
                seq { yield Currency.BTC }
            else
                Currency.GetAll()

        seq {
            for currency in allCurrencies do
                let activeKinds = [AccountKind.ReadOnly; AccountKind.Normal]
                for kind in activeKinds do
                    for accountFile in Config.GetAccountFiles [currency] kind do
                        yield GetAccountFromFile accountFile currency kind
        }


    let GetNormalAccountsPairingInfoForWatchWallet(): Option<WatchWalletInfo> =
        let allCurrencies = Currency.GetAll()

        let utxoCurrencyAccountFiles =
            Config.GetAccountFiles (allCurrencies.Where(fun currency -> currency.IsUtxo())) AccountKind.Normal
        let etherCurrencyAccountFiles =
            Config.GetAccountFiles (allCurrencies.Where(fun currency -> currency.IsEtherBased())) AccountKind.Normal
        if (not (utxoCurrencyAccountFiles.Any())) || (not (etherCurrencyAccountFiles.Any())) then
            None
        else
            let firstUtxoAccountFile = utxoCurrencyAccountFiles.First()
            let utxoCoinPublicKey = UtxoCoin.Account.GetPublicKeyFromNormalAccountFile firstUtxoAccountFile
            let firstEtherAccountFile = etherCurrencyAccountFiles.First()
            let etherPublicAddress = Ether.Account.GetPublicAddressFromNormalAccountFile firstEtherAccountFile
            Some {
                UtxoCoinPublicKey = utxoCoinPublicKey.ToString()
                EtherPublicAddress = etherPublicAddress
            }

    let GetArchivedAccountsWithPositiveBalance (cancelSourceOption: Option<CustomCancelSource>)
                                                   : Async<seq<ArchivedAccount*decimal>> =
        let asyncJobs = seq<Async<ArchivedAccount*Option<decimal>>> {
            let allCurrencies = Currency.GetAll()

            for currency in allCurrencies do
                let fromUnencryptedPrivateKeyToPublicAddressFunc =
                    if currency.IsUtxo() then
                        UtxoCoin.Account.GetPublicAddressFromUnencryptedPrivateKey currency
                    elif currency.IsEtherBased() then
                        Ether.Account.GetPublicAddressFromUnencryptedPrivateKey
                    else
                        failwith <| SPrintF1 "Unknown currency %A" currency

                let fromConfigAccountFileToPublicAddressFunc (accountConfigFile: FileRepresentation) =
                    let privateKeyFromConfigFile = accountConfigFile.Content()
                    fromUnencryptedPrivateKeyToPublicAddressFunc privateKeyFromConfigFile

                for accountFile in Config.GetAccountFiles [currency] AccountKind.Archived do
                    let account = ArchivedAccount(currency, accountFile, fromConfigAccountFileToPublicAddressFunc)
                    let maybeBalanceJob = GetShowableBalanceInternal account ServerSelectionMode.Fast
                    yield async {
                        let! maybeBalance = maybeBalanceJob cancelSourceOption
                        let positiveBalance =
                            match maybeBalance with
                            | Some balance ->
                                if (balance > 0m) then
                                    Some(balance)
                                else
                                    None
                            | _ ->
                                None
                        return account,positiveBalance
                    }
        }
        let executedBalances = Async.Parallel asyncJobs
        async {
            let! accountAndPositiveBalances = executedBalances
            return seq {
                for account,maybePositiveBalance in accountAndPositiveBalances do
                    match maybePositiveBalance with
                    | Some positiveBalance -> yield account,positiveBalance
                    | _ -> ()
            }
        }

    // TODO: add tests for these (just in case address validation breaks after upgrading our dependencies)
    let ValidateAddress (currency: Currency) (address: string): Async<unit> = async {
        if currency.IsEtherBased() then
            do! Ether.Account.ValidateAddress currency address
        elif currency.IsUtxo() then
            UtxoCoin.Account.ValidateAddress currency address
        else
            failwith <| SPrintF1 "Unknown currency %A" currency
    }


    let EstimateFee (account: IAccount) (amount: TransferAmount) destination: Async<IBlockchainFeeInfo> =
        async {
            match account with
            | :? UtxoCoin.IUtxoAccount as utxoAccount ->
                if not (account.Currency.IsUtxo()) then
                    failwith <| SPrintF1 "Currency %A not Utxo-type but account is? report this bug (estimatefee)"
                                         account.Currency
                let! fee = UtxoCoin.Account.EstimateFee utxoAccount amount destination
                return fee :> IBlockchainFeeInfo
            | _ ->
                if not (account.Currency.IsEtherBased()) then
                    failwith <| SPrintF1 "Currency %A not ether based and not UTXO either? not supported, report this bug (estimatefee)"
                        account.Currency
                let! fee = Ether.Account.EstimateFee account amount destination
                return fee :> IBlockchainFeeInfo
        }

    let private SaveOutgoingTransactionInCache transactionProposal (fee: IBlockchainFeeInfo) txId =
        let amountTransferredPlusFeeIfCurrencyFeeMatches =
            if transactionProposal.Amount.BalanceAtTheMomentOfSending = transactionProposal.Amount.ValueToSend
                || transactionProposal.Amount.Currency <> fee.Currency then
                transactionProposal.Amount.ValueToSend
            else
                transactionProposal.Amount.ValueToSend + fee.FeeValue
        Caching.Instance.StoreOutgoingTransaction
            transactionProposal.OriginAddress
            transactionProposal.Amount.Currency
            fee.Currency
            txId
            amountTransferredPlusFeeIfCurrencyFeeMatches
            fee.FeeValue

    // FIXME: if out of gas, miner fee is still spent, we should inspect GasUsed and use it for the call to
    //        SaveOutgoingTransactionInCache
    let private CheckIfOutOfGas (transactionMetadata: IBlockchainFeeInfo) (txHash: string)
                       : Async<unit> =
        async {
            match transactionMetadata with
            | :? Ether.TransactionMetadata as etherTxMetadata ->
                try
                    let! outOfGas = Ether.Server.IsOutOfGas transactionMetadata.Currency txHash etherTxMetadata.Fee.GasLimit
                    if outOfGas then
                        return failwith <| SPrintF1 "Transaction ran out of gas: %s" txHash
                with
                | ex ->
                    return raise <| Exception(SPrintF1 "An issue occurred while trying to check if the following transaction ran out of gas: %s" txHash, ex)
            | _ ->
                ()
        }

    // FIXME: broadcasting shouldn't just get N consistent replies from FaultTolerantClient,
    // but send it to as many as possible, otherwise it could happen that some server doesn't
    // broadcast it even if you sent it
    let BroadcastTransaction (trans: SignedTransaction<_>): Async<Uri> =
        async {
            let currency = trans.TransactionInfo.Proposal.Amount.Currency

            let! txId =
                if currency.IsEtherBased() then
                    Ether.Account.BroadcastTransaction trans
                elif currency.IsUtxo() then
                    UtxoCoin.Account.BroadcastTransaction currency trans
                else
                    failwith <| SPrintF1 "Unknown currency %A" currency

            do! CheckIfOutOfGas trans.TransactionInfo.Metadata txId

            SaveOutgoingTransactionInCache trans.TransactionInfo.Proposal trans.TransactionInfo.Metadata txId

            let uri = BlockExplorer.GetTransaction currency txId
            return uri
        }

    let SignTransaction (account: NormalAccount)
                        (destination: string)
                        (amount: TransferAmount)
                        (transactionMetadata: IBlockchainFeeInfo)
                        (password: string) =

        match transactionMetadata with
        | :? Ether.TransactionMetadata as etherTxMetadata ->
            Ether.Account.SignTransaction
                  account
                  etherTxMetadata
                  destination
                  amount
                  password
        | :? UtxoCoin.TransactionMetadata as btcTxMetadata ->
            match account with
            | :? UtxoCoin.NormalUtxoAccount as utxoAccount ->
                UtxoCoin.Account.SignTransaction
                    utxoAccount
                    btcTxMetadata
                    destination
                    amount
                    password
            | _ ->
                failwith "An UtxoCoin.TransactionMetadata should come with a UtxoCoin.Account"

        | _ -> failwith "fee type unknown"

    let CheckValidPassword (password: string) =
        let checkJobs =
            seq {
                for account in GetAllActiveAccounts().OfType<NormalAccount>() do
                    yield async {
                        if (account :> IAccount).Currency.IsEtherBased() then
                            try
                                Ether.Account.CheckValidPassword account password
                                return true
                            with
                            | :? InvalidPassword ->
                                return false
                        else
                            try
                                UtxoCoin.Account.CheckValidPassword account password
                                return true
                            with
                            | :? InvalidPassword ->
                                return false
                    }
            }
        Async.Parallel checkJobs

    let private CreateArchivedAccount (currency: Currency) (unencryptedPrivateKey: string): ArchivedAccount =
        let fromUnencryptedPrivateKeyToPublicAddressFunc =
            if currency.IsUtxo() then
                UtxoCoin.Account.GetPublicAddressFromUnencryptedPrivateKey currency
            elif currency.IsEther() then
                Ether.Account.GetPublicAddressFromUnencryptedPrivateKey
            else
                failwith <| SPrintF1 "Unknown currency %A" currency

        let fromConfigFileToPublicAddressFunc (accountConfigFile: FileRepresentation) =
            // there's no ETH unencrypted standard: https://github.com/ethereum/wiki/wiki/Web3-Secret-Storage-Definition
            // ... so we simply write the private key in string format
            let privateKeyFromConfigFile = accountConfigFile.Content()
            fromUnencryptedPrivateKeyToPublicAddressFunc privateKeyFromConfigFile

        let fileName = fromUnencryptedPrivateKeyToPublicAddressFunc unencryptedPrivateKey
        let conceptAccount = {
            Currency = currency
            FileRepresentation = { Name = fileName; Content = fun _ -> unencryptedPrivateKey }
            ExtractPublicAddressFromConfigFileFunc = fromConfigFileToPublicAddressFunc
        }
        let newAccountFile = Config.AddAccount conceptAccount AccountKind.Archived
        ArchivedAccount(currency, newAccountFile, fromConfigFileToPublicAddressFunc)

    let Archive (account: NormalAccount)
                (password: string)
                : unit =
        let currency = (account:>IAccount).Currency
        let privateKeyAsString =
            if currency.IsUtxo() then
                let privKey = UtxoCoin.Account.GetPrivateKey account password
                privKey.GetWif(UtxoCoin.Account.GetNetwork currency).ToWif()
            elif currency.IsEther() then
                let privKey = Ether.Account.GetPrivateKey account password
                privKey.GetPrivateKey()
            else
                failwith <| SPrintF1 "Unknown currency %A" currency
        CreateArchivedAccount currency privateKeyAsString
        |> ignore<ArchivedAccount>
        Config.RemoveNormalAccount account

    let SweepArchivedFunds (account: ArchivedAccount)
                           (balance: decimal)
                           (destination: IAccount)
                           (txMetadata: IBlockchainFeeInfo) =
        match txMetadata with
        | :? Ether.TransactionMetadata as etherTxMetadata ->
            Ether.Account.SweepArchivedFunds account balance destination etherTxMetadata
        | :? UtxoCoin.TransactionMetadata as utxoTxMetadata ->
            match account with
            | :? UtxoCoin.ArchivedUtxoAccount as utxoAccount ->
                UtxoCoin.Account.SweepArchivedFunds utxoAccount balance destination utxoTxMetadata
            | _ ->
                failwith "If tx metadata is UTXO type, archived account should be too"
        | _ -> failwith "tx metadata type unknown"

    let SendPayment (account: NormalAccount)
                    (txMetadata: IBlockchainFeeInfo)
                    (destination: string)
                    (amount: TransferAmount)
                    (password: string)
                        : Async<Uri> =
        let baseAccount = account :> IAccount
        if (baseAccount.PublicAddress.Equals(destination, StringComparison.InvariantCultureIgnoreCase)) then
            raise DestinationEqualToOrigin

        let currency = baseAccount.Currency

        async {
            do! ValidateAddress currency destination

            let! txId =
                match txMetadata with
                | :? UtxoCoin.TransactionMetadata as btcTxMetadata ->
                    if not (currency.IsUtxo()) then
                        failwith <| SPrintF1 "Currency %A not Utxo-type but tx metadata is? report this bug (sendpayment)"
                                             currency
                    match account with
                    | :? UtxoCoin.NormalUtxoAccount as utxoAccount ->
                        UtxoCoin.Account.SendPayment utxoAccount btcTxMetadata destination amount password
                    | _ ->
                        failwith "Account not Utxo-type but tx metadata is? report this bug (sendpayment)"

                | :? Ether.TransactionMetadata as etherTxMetadata ->
                    if not (currency.IsEtherBased()) then
                        failwith "Account not ether-type but tx metadata is? report this bug (sendpayment)"
                    Ether.Account.SendPayment account etherTxMetadata destination amount password
                | _ ->
                    failwith "Unknown tx metadata type"

            do! CheckIfOutOfGas txMetadata txId

            let transactionProposal =
                {
                    OriginAddress = baseAccount.PublicAddress
                    Amount = amount
                    DestinationAddress = destination
                }

            SaveOutgoingTransactionInCache transactionProposal txMetadata txId

            let uri = BlockExplorer.GetTransaction currency txId
            return uri
        }

    let SignUnsignedTransaction (account)
                                (unsignedTrans: UnsignedTransaction<IBlockchainFeeInfo>)
                                password =
        let rawTransaction = SignTransaction account
                                 unsignedTrans.Proposal.DestinationAddress
                                 unsignedTrans.Proposal.Amount
                                 unsignedTrans.Metadata
                                 password

        { TransactionInfo = unsignedTrans; RawTransaction = rawTransaction }

    let public ExportSignedTransaction (trans: SignedTransaction<_>) =
        Marshalling.Serialize trans

    let SaveSignedTransaction (trans: SignedTransaction<_>) (filePath: string) =

        let json =
            match trans.TransactionInfo.Metadata.GetType() with
            | t when t = typeof<Ether.TransactionMetadata> ->
                let unsignedEthTx = {
                    Metadata = box trans.TransactionInfo.Metadata :?> Ether.TransactionMetadata;
                    Proposal = trans.TransactionInfo.Proposal;
                    Cache = trans.TransactionInfo.Cache;
                }
                let signedEthTx = {
                    TransactionInfo = unsignedEthTx;
                    RawTransaction = trans.RawTransaction;
                }
                ExportSignedTransaction signedEthTx
            | t when t = typeof<UtxoCoin.TransactionMetadata> ->
                let unsignedBtcTx = {
                    Metadata = box trans.TransactionInfo.Metadata :?> UtxoCoin.TransactionMetadata;
                    Proposal = trans.TransactionInfo.Proposal;
                    Cache = trans.TransactionInfo.Cache;
                }
                let signedBtcTx = {
                    TransactionInfo = unsignedBtcTx;
                    RawTransaction = trans.RawTransaction;
                }
                ExportSignedTransaction signedBtcTx
            | _ -> failwith "Unknown miner fee type"

        File.WriteAllText(filePath, json)

    let CreateReadOnlyAccounts (watchWalletInfo: WatchWalletInfo): Async<unit> = async {
        for etherCurrency in Currency.GetAll().Where(fun currency -> currency.IsEtherBased()) do
            do! ValidateAddress etherCurrency watchWalletInfo.EtherPublicAddress
            let conceptAccountForReadOnlyAccount = {
                Currency = etherCurrency
                FileRepresentation = { Name = watchWalletInfo.EtherPublicAddress; Content = fun _ -> String.Empty }
                ExtractPublicAddressFromConfigFileFunc = (fun file -> file.Name)
            }
            Config.AddAccount conceptAccountForReadOnlyAccount AccountKind.ReadOnly
            |> ignore<FileRepresentation>

        for utxoCurrency in Currency.GetAll().Where(fun currency -> currency.IsUtxo()) do
            let address =
                UtxoCoin.Account.GetPublicAddressFromPublicKey utxoCurrency
                                                               (NBitcoin.PubKey(watchWalletInfo.UtxoCoinPublicKey))
            do! ValidateAddress utxoCurrency address
            let conceptAccountForReadOnlyAccount = {
                Currency = utxoCurrency
                FileRepresentation = { Name = address; Content = fun _ -> watchWalletInfo.UtxoCoinPublicKey }
                ExtractPublicAddressFromConfigFileFunc = (fun file -> file.Name)
            }
            Config.AddAccount conceptAccountForReadOnlyAccount AccountKind.ReadOnly
            |> ignore<FileRepresentation>
    }

    let Remove (account: ReadOnlyAccount) =
        Config.RemoveReadOnlyAccount account

    let private CreateConceptEtherAccountInternal (password: string) (seed: array<byte>)
                                                 : Async<FileRepresentation*(FileRepresentation->string)> =
        async {
            let! virtualFile = Ether.Account.Create password seed
            return virtualFile, Ether.Account.GetPublicAddressFromNormalAccountFile
        }

    let private CreateConceptAccountInternal (currency: Currency) (password: string) (seed: array<byte>)
                                            : Async<FileRepresentation*(FileRepresentation->string)> =
        async {
            if currency.IsUtxo() then
                let! virtualFile = UtxoCoin.Account.Create currency password seed
                return virtualFile, UtxoCoin.Account.GetPublicAddressFromNormalAccountFile currency
            elif currency.IsEtherBased() then
                return! CreateConceptEtherAccountInternal password seed
            else
                return failwith <| SPrintF1 "Unknown currency %A" currency
        }


    let CreateConceptAccount (currency: Currency) (password: string) (seed: array<byte>)
                            : Async<ConceptAccount> =
        async {
            let! virtualFile, fromEncPrivKeyToPublicAddressFunc =
                CreateConceptAccountInternal currency password seed
            return {
                       Currency = currency;
                       FileRepresentation = virtualFile
                       ExtractPublicAddressFromConfigFileFunc = fromEncPrivKeyToPublicAddressFunc;
                   }
        }

    let private CreateConceptAccountAux (currency: Currency) (password: string) (seed: array<byte>)
                            : Async<List<ConceptAccount>> =
        async {
            let! singleAccount = CreateConceptAccount currency password seed
            return singleAccount::List.Empty
        }

    let CreateEtherNormalAccounts (password: string) (seed: array<byte>)
                                      : Async<List<ConceptAccount>> =
        let supportedEtherCurrencies = Currency.GetAll().Where(fun currency -> currency.IsEtherBased())
        let etherAccounts = async {
            let! virtualFile, fromEncPrivKeyToPublicAddressFunc =
                CreateConceptEtherAccountInternal password seed
            return seq {
                for etherCurrency in supportedEtherCurrencies do
                    yield {
                              Currency = etherCurrency;
                              FileRepresentation = virtualFile
                              ExtractPublicAddressFromConfigFileFunc = fromEncPrivKeyToPublicAddressFunc
                          }
            } |> List.ofSeq
        }
        etherAccounts

    let CreateNormalAccount (conceptAccount: ConceptAccount): NormalAccount =
        let newAccountFile = Config.AddAccount conceptAccount AccountKind.Normal
        NormalAccount(conceptAccount.Currency, newAccountFile, conceptAccount.ExtractPublicAddressFromConfigFileFunc)

    let GenerateMasterPrivateKey (passphrase: string)
                                 (dobPartOfSalt: DateTime) (emailPartOfSalt: string)
                                     : Async<array<byte>> =
        async {
            let salt = SPrintF2 "%s+%s" (dobPartOfSalt.Date.ToString("yyyyMMdd")) (emailPartOfSalt.ToLower())
            let privateKeyBytes = WarpKey.CreatePrivateKey passphrase salt
            return privateKeyBytes
        }

    let CreateAllConceptAccounts (privateKeyBytes: array<byte>) (encryptionPassword: string)
                                     : Async<seq<ConceptAccount>> = async {
        let etherAccounts = CreateEtherNormalAccounts encryptionPassword privateKeyBytes
        let nonEthCurrencies = Currency.GetAll().Where(fun currency -> not (currency.IsEtherBased()))

        let nonEtherAccounts: List<Async<List<ConceptAccount>>> =
            seq {
                // TODO: figure out if we can reuse CPU computation of WIF creation between BTC&LTC
                for nonEthCurrency in nonEthCurrencies do
                    yield CreateConceptAccountAux nonEthCurrency encryptionPassword privateKeyBytes
            } |> List.ofSeq

        let allAccounts = etherAccounts::nonEtherAccounts

        let createAllAccountsJob = Async.Parallel allAccounts

        let! allCreatedConceptAccounts = createAllAccountsJob
        let allConceptAccounts =
            seq {
                for accountGroup in allCreatedConceptAccounts do
                    for conceptAccount in accountGroup do
                        yield conceptAccount
            }
        return allConceptAccounts
    }

    let CreateAllAccounts (privateKeyBytes: array<byte>) (encryptionPassword: string): Async<unit> = async {
        let! allConceptAccounts = CreateAllConceptAccounts privateKeyBytes encryptionPassword
        for conceptAccount in allConceptAccounts do
            CreateNormalAccount conceptAccount
            |> ignore<NormalAccount>
    }

    let CheckValidSeed (passphrase: string)
                       (dobPartOfSalt: DateTime)
                       (emailPartOfSalt: string) =
        async {
            let! masterPrivateKey = GenerateMasterPrivateKey passphrase dobPartOfSalt emailPartOfSalt
            let! allConceptAccounts = CreateAllConceptAccounts masterPrivateKey (Guid.NewGuid().ToString())
            return allConceptAccounts.All(fun conceptAccount ->
                GetAllActiveAccounts().Any(fun account ->
                    let publicAddressOfConceptAccount =
                        conceptAccount.ExtractPublicAddressFromConfigFileFunc conceptAccount.FileRepresentation
                    let publicAddressMatches = (account.PublicAddress = publicAddressOfConceptAccount)
                    publicAddressMatches
                )
            )
        }

    let WipeAll() =
        Config.Wipe()
        Caching.Instance.ClearAll()

    let public ExportUnsignedTransactionToJson trans =
        Marshalling.Serialize trans

    let private SerializeUnsignedTransactionPlain (transProposal: UnsignedTransactionProposal)
                                                  (txMetadata: IBlockchainFeeInfo)
                                                      : string =
        let readOnlyAccounts = GetAllActiveAccounts().OfType<ReadOnlyAccount>()

        match txMetadata with
        | :? Ether.TransactionMetadata as etherTxMetadata ->
            Ether.Account.SaveUnsignedTransaction transProposal etherTxMetadata readOnlyAccounts
        | :? UtxoCoin.TransactionMetadata as btcTxMetadata ->
            UtxoCoin.Account.SaveUnsignedTransaction transProposal btcTxMetadata readOnlyAccounts
        | _ -> failwith "fee type unknown"

    let SaveUnsignedTransaction (transProposal: UnsignedTransactionProposal)
                                (txMetadata: IBlockchainFeeInfo)
                                (filePath: string) =
        let json = SerializeUnsignedTransactionPlain transProposal txMetadata
        File.WriteAllText(filePath, json)

    let public ImportUnsignedTransactionFromJson (json: string): UnsignedTransaction<IBlockchainFeeInfo> =

        let transType = Marshalling.ExtractType json

        match transType with
        | _ when transType = typeof<UnsignedTransaction<UtxoCoin.TransactionMetadata>> ->
            let deserializedBtcTransaction: UnsignedTransaction<UtxoCoin.TransactionMetadata> =
                    Marshalling.Deserialize json
            deserializedBtcTransaction.ToAbstract()
        | _ when transType = typeof<UnsignedTransaction<Ether.TransactionMetadata>> ->
            let deserializedBtcTransaction: UnsignedTransaction<Ether.TransactionMetadata> =
                    Marshalling.Deserialize json
            deserializedBtcTransaction.ToAbstract()
        | unexpectedType ->
            raise <| Exception(SPrintF1 "Unknown unsignedTransaction subtype: %s" unexpectedType.FullName)

    let public ImportSignedTransactionFromJson (json: string): SignedTransaction<IBlockchainFeeInfo> =
        let transType = Marshalling.ExtractType json

        match transType with
        | _ when transType = typeof<SignedTransaction<UtxoCoin.TransactionMetadata>> ->
            let deserializedBtcTransaction: SignedTransaction<UtxoCoin.TransactionMetadata> =
                    Marshalling.Deserialize json
            deserializedBtcTransaction.ToAbstract()
        | _ when transType = typeof<SignedTransaction<Ether.TransactionMetadata>> ->
            let deserializedBtcTransaction: SignedTransaction<Ether.TransactionMetadata> =
                    Marshalling.Deserialize json
            deserializedBtcTransaction.ToAbstract()
        | unexpectedType ->
            raise <| Exception(SPrintF1 "Unknown signedTransaction subtype: %s" unexpectedType.FullName)

    let LoadSignedTransactionFromFile (filePath: string) =
        let signedTransInJson = File.ReadAllText(filePath)

        ImportSignedTransactionFromJson signedTransInJson

    let LoadUnsignedTransactionFromFile (filePath: string): UnsignedTransaction<IBlockchainFeeInfo> =
        let unsignedTransInJson = File.ReadAllText(filePath)

        ImportUnsignedTransactionFromJson unsignedTransInJson

    let GetSignedTransactionDetails<'T when 'T :> IBlockchainFeeInfo> (signedTransaction: SignedTransaction<'T>)
                                                                          : ITransactionDetails =
        let currency = signedTransaction.TransactionInfo.Proposal.Amount.Currency
        if currency.IsUtxo () then
            UtxoCoin.Account.GetSignedTransactionDetails
                signedTransaction.RawTransaction
                currency
        elif currency.IsEtherBased () then
            Ether.Account.GetSignedTransactionDetails
                signedTransaction
        else
            failwith <| (SPrintF1 "Unknown currency: %A" currency)

