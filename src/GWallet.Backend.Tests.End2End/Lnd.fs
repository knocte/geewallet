namespace GWallet.Backend.Tests.End2End

open System
open System.IO
open System.Text
open System.Threading.Tasks

open BTCPayServer.Lightning
open BTCPayServer.Lightning.LND
open DotNetLightning.Utils
open ResultUtils.Portability
open NBitcoin

open GWallet.Backend
open GWallet.Backend.UtxoCoin.Lightning
open GWallet.Backend.FSharpUtil.UwpHacks

type Lnd = {
    LndDir: string
    ProcessWrapper: ProcessWrapper
    ConnectionString: string
    ClientFactory: ILightningClientFactory
} with
    interface IDisposable with
        member self.Dispose() =
            self.ProcessWrapper.Process.Kill()
            self.ProcessWrapper.WaitForExit()
            Directory.Delete(self.LndDir, true)

    static member Start(bitcoind: Bitcoind): Async<Lnd> = async {
        let lndDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
        Directory.CreateDirectory lndDir |> ignore
        let processWrapper =
            let args =
                ""
                + " --bitcoin.active"
                + " --bitcoin.regtest"
                + " --bitcoin.node=bitcoind"
                + " --bitcoind.rpcuser=" + bitcoind.RpcUser
                + " --bitcoind.rpcpass=" + bitcoind.RpcPassword
                + " --bitcoind.zmqpubrawblock=tcp://127.0.0.1:28332"
                + " --bitcoind.zmqpubrawtx=tcp://127.0.0.1:28333"
                + " --bitcoind.rpchost=localhost:18554"
                + " --debuglevel=trace"
                + " --listen=127.0.0.2"
                + " --restlisten=127.0.0.2:8080"
                + " --lnddir=" + lndDir
            ProcessWrapper.New
                "lnd"
                args
                Map.empty
                false
        processWrapper.WaitForMessage (fun msg -> msg.EndsWith "password gRPC proxy started at 127.0.0.2:8080")
        let connectionString =
            ""
            + "type=lnd-rest;"
            + "server=https://127.0.0.2:8080;"
            + "allowinsecure=true;"
            + "macaroonfilepath=" + Path.Combine(lndDir, "data/chain/bitcoin/regtest/admin.macaroon")
        let clientFactory = new LightningClientFactory(NBitcoin.Network.RegTest) :> ILightningClientFactory
        let lndClient = clientFactory.Create connectionString :?> LndClient
        let walletPassword = Path.GetRandomFileName()
        let! genSeedResp = Async.AwaitTask <| lndClient.SwaggerClient.GenSeedAsync(null, null)
        let initWalletReq =
            LnrpcInitWalletRequest (
                Wallet_password = Encoding.ASCII.GetBytes walletPassword,
                Cipher_seed_mnemonic = genSeedResp.Cipher_seed_mnemonic
            )

        let! _ = Async.AwaitTask <| lndClient.SwaggerClient.InitWalletAsync initWalletReq
        processWrapper.WaitForMessage (fun msg -> msg.EndsWith "Server listening on 127.0.0.2:9735")
        return {
            LndDir = lndDir
            ProcessWrapper = processWrapper
            ConnectionString = connectionString
            ClientFactory = clientFactory
        }
    }

    member self.Client(): LndClient =
        self.ClientFactory.Create self.ConnectionString :?> LndClient

    member self.GetEndPoint(): Async<NodeEndPoint> = async {
        let client = self.Client()
        let! getInfo = Async.AwaitTask (client.SwaggerClient.GetInfoAsync())
        return NodeEndPoint.Parse Currency.BTC (SPrintF1 "%s@127.0.0.2:9735" getInfo.Identity_pubkey)
    }

    member self.GetDepositAddress(): Async<BitcoinAddress> =
        let client = self.Client()
        (client :> ILightningClient).GetDepositAddress ()
        |> Async.AwaitTask

    member self.GetBlockHeight(): Async<BlockHeight> = async {
        let client = self.Client()
        let! getInfo = Async.AwaitTask (client.SwaggerClient.GetInfoAsync())
        return BlockHeight (uint32 getInfo.Block_height.Value)
    }

    member self.WaitForBlockHeight(blockHeight: BlockHeight): Async<unit> = async {
        let! currentBlockHeight = self.GetBlockHeight()
        if blockHeight > currentBlockHeight then
            self.ProcessWrapper.WaitForMessage <| fun msg ->
                msg.Contains(SPrintF1 "New block: height=%i" blockHeight.Value)
        return ()
    }

    member self.Balance(): Async<Money> = async {
        let client = self.Client()
        let! balance = Async.AwaitTask (client.SwaggerClient.WalletBalanceAsync ())
        return Money(uint64 balance.Confirmed_balance, MoneyUnit.Satoshi)
    }

    member self.WaitForBalance(money: Money): Async<unit> = async {
        let! currentBalance = self.Balance()
        if money > currentBalance then
            self.ProcessWrapper.WaitForMessage <| fun msg ->
                msg.Contains "[walletbalance]"
            return! self.WaitForBalance money
        return ()
    }

    member self.SendCoins(money: Money) (address: BitcoinAddress) (feerate: FeeRatePerKw): Async<TxId> = async {
        let client = self.Client()
        let sendCoinsReq =
            LnrpcSendCoinsRequest (
                Addr = address.ToString(),
                Amount = (money.ToUnit MoneyUnit.Satoshi).ToString(),
                Sat_per_byte = feerate.Value.ToString()
            )
        let! sendCoinsResp = Async.AwaitTask (client.SwaggerClient.SendCoinsAsync sendCoinsReq)
        return TxId <| uint256 sendCoinsResp.Txid
    }

    member self.ConnectTo (nodeEndPoint: NodeEndPoint): Async<unit> =
        let client = self.Client()
        let nodeInfo =
            let pubKey =
                let stringified = nodeEndPoint.NodeId.ToString()
                let unstringified = PubKey stringified
                unstringified
            NodeInfo (pubKey, nodeEndPoint.IPEndPoint.Address.ToString(), nodeEndPoint.IPEndPoint.Port)
        (Async.AwaitTask: Task -> Async<unit>) <| (client :> ILightningClient).ConnectTo nodeInfo

    member self.OpenChannel (nodeEndPoint: NodeEndPoint)
                            (amount: Money)
                            (feeRate: FeeRatePerKw)
                                : Async<Result<unit, OpenChannelResult>> = async {
        let client = self.Client()
        let nodeInfo =
            let pubKey =
                let stringified = nodeEndPoint.NodeId.ToString()
                let unstringified = PubKey stringified
                unstringified
            NodeInfo (pubKey, nodeEndPoint.IPEndPoint.Address.ToString(), nodeEndPoint.IPEndPoint.Port)
        let openChannelReq =
            new OpenChannelRequest (
                NodeInfo = nodeInfo,
                ChannelAmount = amount,
                FeeRate = new FeeRate(Money(uint64 feeRate.Value))
            )
        let! openChannelResponse = Async.AwaitTask <| (client :> ILightningClient).OpenChannel openChannelReq
        match openChannelResponse.Result with
        | OpenChannelResult.Ok -> return Ok ()
        | err -> return Error err
    }
