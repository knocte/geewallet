namespace GWallet.Backend.Tests.End2End

open System
open System.IO
open System.Net
open System.Threading

open NBitcoin
open DotNetLightning.Utils.Primitives

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.UtxoCoin.Lightning
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks

type private WalletInstance (password: string, channelStore: ChannelStore) =
    static let oneWalletAtATime: Semaphore = new Semaphore(1, 1)

    static member New (privateKeyOpt: Option<Key>): Async<WalletInstance> = async {
        oneWalletAtATime.WaitOne() |> ignore

        let password = Path.GetRandomFileName()
        let privateKeyBytes =
            match privateKeyOpt with
            | Some privateKey ->
                privateKey.ToBytes()
            | None ->
                use key = new Key()
                key.ToBytes()

        do! Account.CreateAllAccounts privateKeyBytes password
        let btcAccount =
            let account = Account.GetAllActiveAccounts() |> Seq.filter (fun x -> x.Currency = Currency.BTC) |> Seq.head
            account :?> NormalUtxoAccount
        let channelStore = ChannelStore btcAccount
        return new WalletInstance(password, channelStore)
    }

    interface IDisposable with
        member __.Dispose() =
            Account.WipeAll()

            oneWalletAtATime.Release() |> ignore

    member __.Account: IAccount =
        Account.GetAllActiveAccounts() |> Seq.filter (fun x -> x.Currency = Currency.BTC) |> Seq.head

    member self.Address: BitcoinScriptAddress =
        BitcoinScriptAddress(self.Account.PublicAddress, Network.RegTest)

    member __.Password: string = password
    member __.ChannelStore: ChannelStore = channelStore

    member self.GetBalance(): Async<Money> = async {
        let btcAccount = self.Account :?> NormalUtxoAccount
        let! cachedBalance = Account.GetShowableBalance btcAccount ServerSelectionMode.Analysis None
        match cachedBalance with
        | NotFresh _ ->
            do! Async.Sleep 500
            return! self.GetBalance()
        | Fresh amount -> return Money(amount, MoneyUnit.BTC)
    }

    member self.WaitForBalance (minAmount: Money): Async<Money> = async {
        let btcAccount = self.Account :?> NormalUtxoAccount
        let! cachedBalance = Account.GetShowableBalance btcAccount ServerSelectionMode.Analysis None
        match cachedBalance with
        | Fresh amount when amount < minAmount.ToDecimal MoneyUnit.BTC ->
            do! Async.Sleep 500
            return! self.WaitForBalance minAmount
        | NotFresh _ ->
            do! Async.Sleep 500
            return! self.WaitForBalance minAmount
        | Fresh amount -> return Money(amount, MoneyUnit.BTC)
    }

    member self.WaitForFundingConfirmed (channelId: ChannelIdentifier): Async<unit> =
        let channelInfo = self.ChannelStore.ChannelInfo channelId
        let fundingBroadcastButNotLockedData =
            match channelInfo.Status with
            | ChannelStatus.FundingBroadcastButNotLocked fundingBroadcastButNotLockedData
                -> fundingBroadcastButNotLockedData
            | status -> failwith (SPrintF1 "Unexpected channel status. Expected FundingBroadcastButNotLocked, got %A" status)
        let rec waitForFundingConfirmed() = async {
            let! remainingConfirmations = fundingBroadcastButNotLockedData.GetRemainingConfirmations()
            if remainingConfirmations > 0u then
                do! Async.Sleep 1000
                return! waitForFundingConfirmed()
            else
                // TODO: the backend API doesn't give us any way to avoid
                // the FundingOnChainLocationUnknown error, so just sleep
                // to avoid the race condition. This waiting should really
                // be implemented on the backend anyway.
                do! Async.Sleep 10000
                return ()
        }
        waitForFundingConfirmed()

    member self.FundByMining (bitcoind: Bitcoind)
                             (lnd: Lnd)
                                 : Async<unit> = async {
        do! lnd.FundByMining bitcoind

        // fund geewallet
        let geewalletAccountAmount = Money (25m, MoneyUnit.BTC)
        let! feeRate = ElectrumServer.EstimateFeeRate()
        let! _txid = lnd.SendCoins geewalletAccountAmount self.Address feeRate

        // wait for lnd's transaction to appear in mempool
        while bitcoind.GetTxIdsInMempool().Length = 0 do
            Thread.Sleep 500

        // We want to make sure Geewallet considers the money received.
        // A typical number of blocks that is almost universally considered
        // 100% confirmed, is 6. Therefore we mine 7 blocks. Because we have
        // waited for the transaction to appear in bitcoind's mempool, we
        // can assume that the first of the 7 blocks will include the
        // transaction sending money to Geewallet. The next 6 blocks will
        // bury the first block, so that the block containing the transaction
        // will be 6 deep at the end of the following call to generateBlocks.
        // At that point, the 0.25 regtest coins from the above call to sendcoins
        // are considered arrived to Geewallet.
        let consideredConfirmedAmountOfBlocksPlusOne = BlockHeightOffset32 7u
        bitcoind.GenerateBlocks consideredConfirmedAmountOfBlocksPlusOne self.Address
    }


type ClientWalletInstance private (wallet: WalletInstance, nodeClient: NodeClient) =
    static member New (privateKeyOpt: Option<Key>): Async<ClientWalletInstance> = async {
        let! wallet = WalletInstance.New privateKeyOpt
        let nodeClient =
            Connection.StartClient wallet.ChannelStore wallet.Password
        return new ClientWalletInstance(wallet, nodeClient)
    }

    interface IDisposable with
        member __.Dispose() =
            (wallet :> IDisposable).Dispose()

    member __.Account: IAccount =
        wallet.Account
    member __.Address: BitcoinScriptAddress =
        wallet.Address
    member __.Password: string =
        wallet.Password
    member __.ChannelStore: ChannelStore =
        wallet.ChannelStore
    member __.GetBalance(): Async<Money> =
        wallet.GetBalance()
    member __.WaitForBalance(minAmount: Money): Async<Money> =
        wallet.WaitForBalance minAmount
    member __.WaitForFundingConfirmed(channelId: ChannelIdentifier): Async<unit> =
        wallet.WaitForFundingConfirmed channelId
    member __.FundByMining (bitcoind: Bitcoind)
                           (lnd: Lnd)
                               : Async<unit> =
        wallet.FundByMining bitcoind lnd

    member __.NodeClient: NodeClient = nodeClient

    member self.OpenChannelWithFundee (bitcoind: Bitcoind) (nodeEndPoint: NodeEndPoint)
                                          : Async<ChannelIdentifier*Money> = async {
        let fundingAmount = Money(0.1m, MoneyUnit.BTC)
        let! transferAmount = async {
            let! accountBalance = self.WaitForBalance fundingAmount
            return TransferAmount (fundingAmount.ToDecimal MoneyUnit.BTC, accountBalance.ToDecimal MoneyUnit.BTC, Currency.BTC)
        }
        let! metadata = ChannelManager.EstimateChannelOpeningFee (self.Account :?> NormalUtxoAccount) transferAmount
        let! pendingChannelRes =
            Lightning.Network.OpenChannel
                self.NodeClient
                nodeEndPoint
                transferAmount
                metadata
                self.Password
        let pendingChannel = UnwrapResult pendingChannelRes "OpenChannel failed"
        let minimumDepth = (pendingChannel :> IChannelToBeOpened).ConfirmationsRequired
        let channelId = (pendingChannel :> IChannelToBeOpened).ChannelId
        let! fundingTxIdRes = pendingChannel.Accept()
        let _fundingTxId = UnwrapResult fundingTxIdRes "pendingChannel.Accept failed"
        bitcoind.GenerateBlocks (BlockHeightOffset32 minimumDepth) self.Address

        do! self.WaitForFundingConfirmed channelId

        let! lockFundingRes = Lightning.Network.ConnectLockChannelFunding self.NodeClient channelId
        UnwrapResult lockFundingRes "LockChannelFunding failed"

        let channelInfo = self.ChannelStore.ChannelInfo channelId
        match channelInfo.Status with
        | ChannelStatus.Active -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

        if Money(channelInfo.Balance, MoneyUnit.BTC) <> fundingAmount then
            failwith "balance does not match funding amount"

        return channelId, fundingAmount
    }


type ServerWalletInstance private (wallet: WalletInstance, nodeServer: NodeServer) =
    static member New (listenEndpoint: IPEndPoint) (privateKeyOpt: Option<Key>): Async<ServerWalletInstance> = async {
        let! wallet = WalletInstance.New privateKeyOpt
        let nodeServer =
            Connection.StartServer wallet.ChannelStore wallet.Password listenEndpoint
        return new ServerWalletInstance(wallet, nodeServer)
    }

    interface IDisposable with
        member __.Dispose() =
            (wallet :> IDisposable).Dispose()

    member __.Account: IAccount =
        wallet.Account
    member __.Address: BitcoinScriptAddress =
        wallet.Address
    member __.Password: string =
        wallet.Password
    member __.ChannelStore: ChannelStore =
        wallet.ChannelStore
    member __.GetBalance(): Async<Money> =
        wallet.GetBalance()
    member __.WaitForBalance(minAmount: Money): Async<Money> =
        wallet.WaitForBalance minAmount
    member __.WaitForFundingConfirmed(channelId: ChannelIdentifier): Async<unit> =
        wallet.WaitForFundingConfirmed channelId
    member __.FundByMining (bitcoind: Bitcoind)
                           (lnd: Lnd)
                               : Async<unit> =
        wallet.FundByMining bitcoind lnd

    member __.NodeServer: NodeServer = nodeServer
    member self.NodeEndPoint =
        Lightning.Network.EndPoint self.NodeServer

