namespace GWallet.Backend.Ether

// NOTE: we can rename this file to less redundant "Account.fs" when this F# compiler bug is fixed:
// https://github.com/Microsoft/visualfsharp/issues/3231

open System
open System.Numerics
open System.Threading.Tasks

open Nethereum.Signer
open Nethereum.KeyStore
open Nethereum.Util
open Nethereum.KeyStore.Crypto

open GWallet.Backend
open GWallet.Backend.FSharpUtil.UwpHacks

module internal Account =

    let private addressUtil = AddressUtil()
    let private signer = TransactionSigner()

    let private KeyStoreService = KeyStoreService()

    let GetPublicAddressFromUnencryptedPrivateKey (privateKey: string) =
        (EthECKey privateKey).GetPublicAddress()

    let internal GetPublicAddressFromNormalAccountFile (accountFile: FileRepresentation): string =
        let encryptedPrivateKey = accountFile.Content()
        let rawPublicAddress = KeyStoreService.GetAddressFromKeyStore encryptedPrivateKey
        let publicAddress =
            if rawPublicAddress.StartsWith "0x" then
                rawPublicAddress
            else
                "0x" + rawPublicAddress
        publicAddress

    let internal GetAccountFromFile (accountFile: FileRepresentation) (currency: Currency) kind: IAccount =
        if not (currency.IsEtherBased()) then
            failwith <| SPrintF1 "Assertion failed: currency %A should be Ether-type" currency
        match kind with
        | AccountKind.ReadOnly ->
            ReadOnlyAccount(currency, accountFile, fun accountFile -> accountFile.Name) :> IAccount
        | AccountKind.Normal ->
            NormalAccount(currency, accountFile, GetPublicAddressFromNormalAccountFile) :> IAccount
        | _ ->
            failwith <| SPrintF1 "Kind (%A) not supported for this API" kind

    let private GetBalance (account: IAccount)
                           (mode: ServerSelectionMode)
                           (balType: BalanceType)
                           (cancelSourceOption: Option<CustomCancelSource>)
                               = async {
        let! balance =
            if account.Currency.IsEther() then
                Server.GetEtherBalance account.Currency account.PublicAddress balType mode cancelSourceOption
            elif account.Currency.IsEthToken() then
                Server.GetTokenBalance account.Currency account.PublicAddress balType mode cancelSourceOption
            else
                failwith <| SPrintF1 "Assertion failed: currency %A should be Ether or Ether token" account.Currency
        return balance
    }

    let private GetBalanceFromServer (account: IAccount)
                                     (balType: BalanceType)
                                     (mode: ServerSelectionMode)
                                     (cancelSourceOption: Option<CustomCancelSource>)
                                         : Async<Option<decimal>> =
        async {
            try
                let! balance = GetBalance account mode balType cancelSourceOption
                return Some balance
            with
            | ex when (FSharpUtil.FindException<ResourceUnavailabilityException> ex).IsSome ->
                return None
        }

    let internal GetShowableBalance (account: IAccount)
                                    (mode: ServerSelectionMode)
                                    (cancelSourceOption: Option<CustomCancelSource>)
                                        : Async<Option<decimal>> =
        let getBalanceWithoutCaching(maybeUnconfirmedBalanceTaskAlreadyStarted: Option<Task<Option<decimal>>>)
                : Async<Option<decimal>> =
            async {
                let! confirmed = GetBalanceFromServer account BalanceType.Confirmed mode cancelSourceOption
                if mode = ServerSelectionMode.Fast then
                    return confirmed
                else
                    let! unconfirmed =
                        match maybeUnconfirmedBalanceTaskAlreadyStarted with
                        | None ->
                            GetBalanceFromServer account BalanceType.Confirmed mode cancelSourceOption
                        | Some unconfirmedBalanceTask ->
                            Async.AwaitTask unconfirmedBalanceTask

                    match unconfirmed,confirmed with
                    | Some unconfirmedAmount,Some confirmedAmount ->
                        if unconfirmedAmount < confirmedAmount then
                            return unconfirmed
                        else
                            return confirmed
                    | _ -> return confirmed
            }

        async {
            if Caching.Instance.FirstRun then
                return! getBalanceWithoutCaching None
            else
                let unconfirmedJob = GetBalanceFromServer account BalanceType.Confirmed mode cancelSourceOption
                let! cancellationToken = Async.CancellationToken
                let unconfirmedTask = Async.StartAsTask(unconfirmedJob, ?cancellationToken = Some cancellationToken)
                let maybeCachedBalance = Caching.Instance.RetrieveLastCompoundBalance account.PublicAddress account.Currency
                match maybeCachedBalance with
                | NotAvailable ->
                    return! getBalanceWithoutCaching(Some unconfirmedTask)
                | Cached(cachedBalance,_) ->
                    let! unconfirmed = Async.AwaitTask unconfirmedTask
                    match unconfirmed with
                    | Some unconfirmedAmount ->
                        if unconfirmedAmount <= cachedBalance then
                            return unconfirmed
                        else
                            return! getBalanceWithoutCaching(Some unconfirmedTask)
                    | None ->
                        return! getBalanceWithoutCaching(Some unconfirmedTask)
        }

    let ValidateAddress (currency: Currency) (address: string) = async {
        if String.IsNullOrEmpty address then
            raise <| ArgumentNullException "address"

        let ETHEREUM_ADDRESSES_LENGTH = 42
        let ETHEREUM_ADDRESS_PREFIX = "0x"

        if not (address.StartsWith ETHEREUM_ADDRESS_PREFIX) then
            raise <| AddressMissingProperPrefix [ ETHEREUM_ADDRESS_PREFIX ]

        if address.Length <> ETHEREUM_ADDRESSES_LENGTH then
            raise <| AddressWithInvalidLength [ ETHEREUM_ADDRESSES_LENGTH ]

        do! Ether.Server.CheckIfAddressIsAValidPaymentDestination currency address

        if not (addressUtil.IsChecksumAddress address) then
            let validCheckSumAddress = addressUtil.ConvertToChecksumAddress(address)
            raise <| AddressWithInvalidChecksum (Some validCheckSumAddress)
    }

    let private GetTransactionCount (currency: Currency) (publicAddress: string): Async<int64> = async {
        let! result = Ether.Server.GetTransactionCount currency publicAddress
        let value = result.Value
        if value > BigInteger Int64.MaxValue then
            failwith <| SPrintF1 "Serialization doesn't support such a big integer (%s) for the nonce, please report this issue."
                      (result.ToString())
        let int64result:Int64 = BigInteger.op_Explicit value
        return int64result
    }

    let private GetGasPrice currency: Async<int64> = async {
        let! gasPrice = Ether.Server.GetGasPrice currency
        if gasPrice.Value > BigInteger Int64.MaxValue then
            failwith <| SPrintF1 "Serialization doesn't support such a big integer (%s) for the gas, please report this issue."
                      (gasPrice.Value.ToString())
        let gasPrice64: Int64 = BigInteger.op_Explicit gasPrice.Value
        return gasPrice64
    }

    let private GAS_COST_FOR_A_NORMAL_ETHER_TRANSACTION:int64 = 21000L

    let EstimateEtherTransferFee (account: IAccount) (amount: TransferAmount): Async<TransactionMetadata> = async {
        let! gasPrice64 = GetGasPrice account.Currency
        let initialEthMinerFee = MinerFee(GAS_COST_FOR_A_NORMAL_ETHER_TRANSACTION, gasPrice64, DateTime.UtcNow, account.Currency)
        let! txCount = GetTransactionCount account.Currency account.PublicAddress

        let! maybeExchangeRate = FiatValueEstimation.UsdValue amount.Currency
        let maybeBetterFee =
            match maybeExchangeRate with
            | NotFresh NotAvailable -> initialEthMinerFee
            | NotFresh (Cached (exchangeRate,_)) | Fresh exchangeRate ->
                MinerFee.GetHigherFeeThanRidiculousFee exchangeRate
                                                       initialEthMinerFee
                                                       0.01m

        let feeValue = maybeBetterFee.CalculateAbsoluteValue()
        if (amount.ValueToSend <> amount.BalanceAtTheMomentOfSending) &&
            feeValue > (amount.BalanceAtTheMomentOfSending - amount.ValueToSend) then
            raise <| InsufficientBalanceForFee (Some feeValue)

        return { Ether.Fee = maybeBetterFee; Ether.TransactionCount = txCount }
    }

    // FIXME: this should raise InsufficientBalanceForFee
    let EstimateTokenTransferFee (account: IAccount) amount destination: Async<TransactionMetadata> = async {
        let! gasPrice64 = GetGasPrice account.Currency

        let baseCurrency =
            match account.Currency with
            | DAI | SAI -> ETH
            | _ -> failwith <| SPrintF1 "Unknown token %A" account.Currency

        let! tokenTransferFee = Ether.Server.EstimateTokenTransferFee account amount destination
        if tokenTransferFee.Value > BigInteger Int64.MaxValue then
            failwith <| SPrintF1 "Serialization doesn't support such a big integer (%s) for the gas cost of the token transfer, please report this issue."
                      (tokenTransferFee.Value.ToString())
        let gasCost64: Int64 = BigInteger.op_Explicit tokenTransferFee.Value

        let ethMinerFee = MinerFee(gasCost64, gasPrice64, DateTime.UtcNow, baseCurrency)
        let! txCount = GetTransactionCount account.Currency account.PublicAddress
        return { Ether.Fee = ethMinerFee; Ether.TransactionCount = txCount }
    }

    let EstimateFee (account: IAccount) (amount: TransferAmount) destination: Async<TransactionMetadata> = async {
        if account.Currency.IsEther() then
            return! EstimateEtherTransferFee account amount
        elif account.Currency.IsEthToken() then
            return! EstimateTokenTransferFee account amount.ValueToSend destination
        else
            return failwith <| SPrintF1 "Assertion failed: currency %A should be Ether or Ether token" account.Currency
    }

    let private BroadcastRawTransaction (currency: Currency) trans =
        Ether.Server.BroadcastTransaction currency ("0x" + trans)

    let BroadcastTransaction (trans: SignedTransaction<_>) =
        BroadcastRawTransaction
            trans.TransactionInfo.Proposal.Amount.Currency
            trans.RawTransaction

    let internal GetPrivateKey (account: NormalAccount) password =
        let encryptedPrivateKey = account.GetEncryptedPrivateKey()
        let privKeyInBytes =
            try
                KeyStoreService.DecryptKeyStoreFromJson(password, encryptedPrivateKey)
            with
            | :? DecryptionException ->
                raise InvalidPassword

        EthECKey(privKeyInBytes, true)

    let private GetNetwork (currency: Currency) =
        if not (currency.IsEtherBased()) then
            failwith <| SPrintF1 "Assertion failed: currency %A should be Ether-type" currency
        if currency.IsEthToken() || currency = ETH then
            Config.EthNet
        elif currency = ETC then
            Config.EtcNet
        else
            failwith <| SPrintF1 "Assertion failed: Ether currency %A not supported?" currency

    let private SignEtherTransaction (currency: Currency)
                                     (txMetadata: TransactionMetadata)
                                     (destination: string)
                                     (amount: TransferAmount)
                                     (privateKey: EthECKey) =

        let chain = GetNetwork currency
        if GetNetwork txMetadata.Fee.Currency <> chain then
            invalidArg "chain" (SPrintF2 "Assertion failed: fee currency (%A) chain doesn't match with passed chain (%A)"
                                        txMetadata.Fee.Currency chain)

        let amountToSendConsideringMinerFee =
            if amount.ValueToSend = amount.BalanceAtTheMomentOfSending then
                amount.ValueToSend - (txMetadata :> IBlockchainFeeInfo).FeeValue
            else
                amount.ValueToSend
        let amountInWei = UnitConversion.Convert.ToWei(amountToSendConsideringMinerFee,
                                                       UnitConversion.EthUnit.Ether)

        let privKeyInBytes = privateKey.GetPrivateKeyAsBytes()
        let trans = signer.SignTransaction(
                        privKeyInBytes,
                        chain,
                        destination,
                        amountInWei,
                        BigInteger(txMetadata.TransactionCount),

                        // we use the SignTransaction() overload that has these 2 arguments because if we don't, we depend on
                        // how well the defaults are of Geth node we're connected to, e.g. with the myEtherWallet server I
                        // was trying to spend 0.002ETH from an account that had 0.01ETH and it was always failing with the
                        // "Insufficient Funds" error saying it needed 212,000,000,000,000,000 wei (0.212 ETH)...
                        BigInteger(txMetadata.Fee.GasPriceInWei),
                        BigInteger(txMetadata.Fee.GasLimit))
        trans

    let private SignEtherTokenTransaction (currency: Currency)
                                          (txMetadata: TransactionMetadata)
                                          (origin: string)
                                          (destination: string)
                                          (amount: TransferAmount)
                                          (privateKey: EthECKey) =

        let chain = GetNetwork currency
        let privKeyInBytes = privateKey.GetPrivateKeyAsBytes()

        let amountInWei = UnitConversion.Convert.ToWei(amount.ValueToSend, UnitConversion.EthUnit.Ether)
        let gasLimit = BigInteger(txMetadata.Fee.GasLimit)
        let data = (TokenManager.OfflineTokenServiceWrapper currency)
                       .ComposeInputDataForTransferTransaction(origin,
                                                               destination,
                                                               amountInWei,
                                                               gasLimit)

        let etherValue = BigInteger(0)
        let nonce = BigInteger(txMetadata.TransactionCount)
        let gasPrice = BigInteger(txMetadata.Fee.GasPriceInWei)
        let contractAddress = TokenManager.GetTokenContractAddress currency

        signer.SignTransaction (privKeyInBytes,
                                chain,
                                contractAddress,
                                etherValue,
                                nonce,
                                gasPrice,
                                gasLimit,
                                data)

    let private SignTransactionWithPrivateKey (account: IAccount)
                                              (txMetadata: TransactionMetadata)
                                              (destination: string)
                                              (amount: TransferAmount)
                                              (privateKey: EthECKey) =

        let trans =
            if account.Currency.IsEthToken() then
                SignEtherTokenTransaction
                    account.Currency txMetadata account.PublicAddress destination amount privateKey
            elif account.Currency.IsEtherBased() then
                if txMetadata.Fee.Currency <> account.Currency then
                    failwith <| SPrintF2 "Assertion failed: fee currency (%A) doesn't match with passed chain (%A)"
                              txMetadata.Fee.Currency account.Currency
                SignEtherTransaction account.Currency txMetadata destination amount privateKey
            else
                failwith <| SPrintF1 "Assertion failed: Ether currency %A not supported?" account.Currency

        let chain = GetNetwork account.Currency
        if not (signer.VerifyTransaction(trans, chain)) then
            failwith "Transaction could not be verified?"
        trans

    let SignTransaction (account: NormalAccount)
                        (txMetadata: TransactionMetadata)
                        (destination: string)
                        (amount: TransferAmount)
                        (password: string) =

        let privateKey = GetPrivateKey account password
        SignTransactionWithPrivateKey account txMetadata destination amount privateKey

    let CheckValidPassword (account: NormalAccount) (password: string) =
        GetPrivateKey account password
        |> ignore<EthECKey>

    let SweepArchivedFunds (account: ArchivedAccount)
                           (balance: decimal)
                           (destination: IAccount)
                           (txMetadata: TransactionMetadata) =
        let accountFrom = (account:>IAccount)
        let amount = TransferAmount(balance, balance, accountFrom.Currency)
        let ecPrivKey = EthECKey(account.GetUnencryptedPrivateKey())
        let signedTrans = SignTransactionWithPrivateKey
                              account txMetadata destination.PublicAddress amount ecPrivKey
        BroadcastRawTransaction accountFrom.Currency signedTrans

    let SendPayment (account: NormalAccount)
                    (txMetadata: TransactionMetadata)
                    (destination: string)
                    (amount: TransferAmount)
                    (password: string) =
        let baseAccount = account :> IAccount
        if baseAccount.PublicAddress.Equals (destination, StringComparison.InvariantCultureIgnoreCase) then
            raise DestinationEqualToOrigin

        let currency = baseAccount.Currency

        let trans = SignTransaction account txMetadata destination amount password

        BroadcastRawTransaction currency trans

    let private CreateInternal (password: string) (seed: array<byte>): FileRepresentation =
        let privateKey = EthECKey(seed, true)
        let publicAddress = privateKey.GetPublicAddress()
        if not (addressUtil.IsChecksumAddress(publicAddress)) then
            failwith ("Nethereum's GetPublicAddress gave a non-checksum address: " + publicAddress)

        let accountSerializedJson =
            KeyStoreService.EncryptAndGenerateDefaultKeyStoreAsJson(password,
                                                                    seed,
                                                                    publicAddress)
        let fileNameForAccount = KeyStoreService.GenerateUTCFileName(publicAddress)

        {
            Name = fileNameForAccount
            Content = fun _ -> accountSerializedJson
        }

    let Create (password: string) (seed: array<byte>): Async<FileRepresentation> =
        async {
            return CreateInternal password seed
        }

    let public ExportUnsignedTransactionToJson trans =
        Marshalling.Serialize trans

    let SaveUnsignedTransaction (transProposal: UnsignedTransactionProposal)
                                (txMetadata: TransactionMetadata)
                                (readOnlyAccounts: seq<ReadOnlyAccount>)
                                    : string =

        let unsignedTransaction =
            {
                Proposal = transProposal;
                Cache = Caching.Instance.GetLastCachedData().ToDietCache readOnlyAccounts;
                Metadata = txMetadata;
            }
        ExportUnsignedTransactionToJson unsignedTransaction

