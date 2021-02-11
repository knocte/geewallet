namespace GWallet.Backend.Tests.End2End

open System
open System.IO
open System.Net
open System.Threading

open NBitcoin

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.UtxoCoin.Lightning
open GWallet.Backend.FSharpUtil.UwpHacks

type WalletInstance private (password: string, channelStore: ChannelStore, nodeServer: NodeServer) =
    static let oneWalletAtATime: Semaphore = new Semaphore(1, 1)

    static member internal AccountPrivateKeyToNodeSecret (accountKey: Key) =
        ExtKey (accountKey.ToBytes())

    static member New (listenEndpointOpt: Option<IPEndPoint>) (privateKeyOpt: Option<Key>): Async<WalletInstance> = async {
        oneWalletAtATime.WaitOne() |> ignore

        let password = Path.GetRandomFileName()
        let privateKeyBytes =
            let privateKey =
                match privateKeyOpt with
                | Some privateKey -> privateKey
                | None -> new Key()
            privateKey.ToBytes()

        do! Account.CreateAllAccounts privateKeyBytes password
        let btcAccount =
            let account = Account.GetAllActiveAccounts() |> Seq.filter (fun x -> x.Currency = Currency.BTC) |> Seq.head
            account :?> NormalUtxoAccount
        let channelStore = ChannelStore btcAccount
        let nodeServer =
            let listenEndpoint =
                match listenEndpointOpt with
                | Some listenEndpoint -> listenEndpoint
                | None -> IPEndPoint(IPAddress.Parse "127.0.0.1", 0)
            Connection.StartServer channelStore password listenEndpoint
        return new WalletInstance(password, channelStore, nodeServer)
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
    member __.NodeServer: NodeServer = nodeServer
    member self.NodeEndPoint =
        Lightning.Network.EndPoint self.NodeServer

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
