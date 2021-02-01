namespace GWallet.Backend.Tests

open Microsoft.FSharp.Linq.NullableOperators // For <>?

open Newtonsoft.Json // For JsonConvert

open NUnit.Framework

open GWallet.Backend

open System
open System.IO // For File.WriteAllText
open System.Diagnostics // For Process
open System.Net // For IPAddress and IPEndPoint
open System.Text // For Encoding
open System.Threading // For AutoResetEvent and CancellationToken
open System.Threading.Tasks // For Task
open System.Collections.Concurrent

open NBitcoin // For ExtKey
open ResultUtils.Portability
open BTCPayServer.Lightning
open BTCPayServer.Lightning.LND
open DotNetLightning.Utils

open GWallet.Backend.UtxoCoin // For NormalUtxoAccount
open GWallet.Backend.UtxoCoin.Lightning
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks

type ProcessWrapper = {
    Name: string
    Process: Process
    Queue: ConcurrentQueue<string>
    Semaphore: Semaphore
} with
    static member New (name: string)
                      (arguments: string)
                      (environment: Map<string, string>)
                      (isPython: bool)
                          : ProcessWrapper =

        let fileName =
            let environmentPath = System.Environment.GetEnvironmentVariable "PATH"
            let pathSeparator = Path.PathSeparator
            let paths = environmentPath.Split pathSeparator
            let isWin = Path.DirectorySeparatorChar = '\\'
            let exeName =
                if isWin then
                    name + if isPython then ".py" else ".exe"
                else
                    name
            let paths = [ for x in paths do yield Path.Combine(x, exeName) ]
            let matching = paths |> List.filter File.Exists
            match matching with
            | first :: _ -> first
            | _ ->
                failwith <|
                    SPrintF3
                        "Couldn't find %s in path, tried %A, these paths matched: %A"
                        exeName
                        [ for x in paths do yield (File.Exists x, x) ]
                        matching

        let queue = ConcurrentQueue()
        let semaphore = new Semaphore(0, Int32.MaxValue)
        let startInfo =
            ProcessStartInfo (
                UseShellExecute = false,
                FileName = fileName,
                Arguments = arguments,
                RedirectStandardOutput = true,
                RedirectStandardError = true
            )
        for kvp in environment do
            startInfo.Environment.[kvp.Key] <- kvp.Value
        let proc = new Process()
        proc.StartInfo <- startInfo
        let firstStreamEnded = ref false
        let outputHandler (_: obj) (args: DataReceivedEventArgs) =
            lock firstStreamEnded <| fun () ->
                match args.Data with
                | null ->
                    // We need to wait for both streams (stdout and stderr) to
                    // end. So output has ended and the process has exited
                    // after the second null.
                    if not !firstStreamEnded then
                        firstStreamEnded := true
                    else
                        Console.WriteLine(SPrintF2 "%s (%i) <exited>" name proc.Id)
                        semaphore.Release() |> ignore
                | text ->
                    Console.WriteLine(SPrintF3 "%s (%i): %s" name proc.Id text)
                    queue.Enqueue text
                    semaphore.Release() |> ignore
        proc.OutputDataReceived.AddHandler(DataReceivedEventHandler outputHandler)
        proc.ErrorDataReceived.AddHandler(DataReceivedEventHandler outputHandler)
        proc.EnableRaisingEvents <- true
        if not(proc.Start()) then
            failwith "failed to start process"
        AppDomain.CurrentDomain.ProcessExit.AddHandler(EventHandler (fun _ _ -> proc.Close()))
        proc.BeginOutputReadLine()
        proc.BeginErrorReadLine()
        {
            Name = name
            Process = proc
            Queue = queue
            Semaphore = semaphore
        }

    member this.WaitForMessage(msgFilter: string -> bool) =
        this.Semaphore.WaitOne() |> ignore
        let running, line = this.Queue.TryDequeue()
        if running then
            if msgFilter line then
                ()
            else
                this.WaitForMessage msgFilter
        else
            failwith (this.Name + " exited without outputting message")

    member this.WaitForExit() =
        this.Semaphore.WaitOne() |> ignore
        let running, _ = this.Queue.TryDequeue()
        if running then
            this.WaitForExit()

    member this.ReadToEnd(): list<string> =
        let rec fold (lines: list<string>) =
            this.Semaphore.WaitOne() |> ignore
            let running, line = this.Queue.TryDequeue()
            if running then
                fold <| List.append lines [line]
            else
                lines
        fold List.empty

type Bitcoind = {
    DataDir: string
    RpcUser: string
    RpcPassword: string
    ProcessWrapper: ProcessWrapper
} with
    interface IDisposable with
        member this.Dispose() =
            this.ProcessWrapper.Process.Kill()
            this.ProcessWrapper.WaitForExit()
            Directory.Delete(this.DataDir, true)

    static member Start(): Bitcoind =
        let dataDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
        Directory.CreateDirectory dataDir |> ignore
        let rpcUser = Path.GetRandomFileName()
        let rpcPassword = Path.GetRandomFileName()
        let confPath = Path.Combine(dataDir, "bitcoin.conf")
        File.WriteAllText(
            confPath,
            SPrintF2
                "\
                txindex=1\n\
                printtoconsole=1\n\
                rpcuser=%s\n\
                rpcpassword=%s\n\
                rpcallowip=127.0.0.1\n\
                zmqpubrawblock=tcp://127.0.0.1:28332\n\
                zmqpubrawtx=tcp://127.0.0.1:28333\n\
                fallbackfee=0.00001\n\
                [regtest]\n\
                rpcbind=127.0.0.1\n\
                rpcport=18554"
                rpcUser
                rpcPassword
        )

        let processWrapper =
            ProcessWrapper.New
                "bitcoind"
                (SPrintF1 "-regtest -datadir=%s" dataDir)
                Map.empty
                false
        processWrapper.WaitForMessage (fun msg -> msg.EndsWith "init message: Done loading")
        {
            DataDir = dataDir
            RpcUser = rpcUser
            RpcPassword = rpcPassword
            ProcessWrapper = processWrapper
        }

    member this.GenerateBlocks (number: BlockHeightOffset32) (address: BitcoinAddress) =
        let bitcoinCli =
            ProcessWrapper.New
                "bitcoin-cli"
                (SPrintF3 "-regtest -datadir=%s generatetoaddress %i %s" this.DataDir number.Value (address.ToString()))
                Map.empty
                false
        bitcoinCli.WaitForExit()

    member this.GetTxIdsInMempool(): list<TxId> =
        let bitcoinCli =
            ProcessWrapper.New
                "bitcoin-cli"
                (SPrintF1 "-regtest -datadir=%s getrawmempool" this.DataDir)
                Map.empty
                false
        let lines = bitcoinCli.ReadToEnd()
        let output = String.concat "\n" lines
        let txIdList = JsonConvert.DeserializeObject<list<string>> output
        List.map (fun (txIdString: string) -> TxId <| uint256 txIdString) txIdList

    member this.RpcUrl: string =
        SPrintF2 "http://%s:%s@127.0.0.1:18554" this.RpcUser this.RpcPassword

type ElectrumServer = {
    DbDir: string
    ProcessWrapper: ProcessWrapper
} with
    interface IDisposable with
        member this.Dispose() =
            this.ProcessWrapper.Process.Kill()
            this.ProcessWrapper.WaitForExit()
            Directory.Delete(this.DbDir, true)

    static member Start(bitcoind: Bitcoind): ElectrumServer =
        let dbDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
        Directory.CreateDirectory dbDir |> ignore
        let processWrapper =
            ProcessWrapper.New
                "electrumx_server"
                ""
                (Map.ofList <| [
                    "SERVICES", "tcp://[::1]:50001";
                    "COIN", "BitcoinSegwit";
                    "NET", "regtest";
                    "DAEMON_URL", bitcoind.RpcUrl;
                    "DB_DIRECTORY", dbDir
                ])
                true
        processWrapper.WaitForMessage (fun msg -> msg.EndsWith "TCP server listening on [::1]:50001")
        {
            DbDir = dbDir
            ProcessWrapper = processWrapper
        }

type Lnd = {
    LndDir: string
    ProcessWrapper: ProcessWrapper
    ConnectionString: string
    ClientFactory: ILightningClientFactory
} with
    interface IDisposable with
        member this.Dispose() =
            this.ProcessWrapper.Process.Kill()
            this.ProcessWrapper.WaitForExit()
            Directory.Delete(this.LndDir, true)

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

    member this.Client(): LndClient =
        this.ClientFactory.Create this.ConnectionString :?> LndClient

    member this.GetEndPoint(): Async<NodeEndPoint> = async {
        let client = this.Client()
        let! getInfo = Async.AwaitTask (client.SwaggerClient.GetInfoAsync())
        return NodeEndPoint.Parse Currency.BTC (SPrintF1 "%s@127.0.0.2:9735" getInfo.Identity_pubkey)
    }

    member this.GetDepositAddress(): Async<BitcoinAddress> =
        let client = this.Client()
        (client :> ILightningClient).GetDepositAddress ()
        |> Async.AwaitTask

    member this.GetBlockHeight(): Async<BlockHeight> = async {
        let client = this.Client()
        let! getInfo = Async.AwaitTask (client.SwaggerClient.GetInfoAsync())
        return BlockHeight (uint32 getInfo.Block_height.Value)
    }

    member this.WaitForBlockHeight(blockHeight: BlockHeight): Async<unit> = async {
        let! currentBlockHeight = this.GetBlockHeight()
        if blockHeight > currentBlockHeight then
            this.ProcessWrapper.WaitForMessage <| fun msg ->
                msg.Contains(SPrintF1 "New block: height=%i" blockHeight.Value)
        return ()
    }

    member this.Balance(): Async<Money> = async {
        let client = this.Client()
        let! balance = Async.AwaitTask (client.SwaggerClient.WalletBalanceAsync ())
        return Money(uint64 balance.Confirmed_balance, MoneyUnit.Satoshi)
    }

    member this.WaitForBalance(money: Money): Async<unit> = async {
        let! currentBalance = this.Balance()
        if money > currentBalance then
            this.ProcessWrapper.WaitForMessage <| fun msg ->
                msg.Contains "[walletbalance]"
            return! this.WaitForBalance money
        return ()
    }
    
    member this.SendCoins(money: Money) (address: BitcoinAddress) (feerate: FeeRatePerKw): Async<TxId> = async {
        let client = this.Client()
        let sendCoinsReq =
            LnrpcSendCoinsRequest (
                Addr = address.ToString(),
                Amount = (money.ToUnit MoneyUnit.Satoshi).ToString(),
                Sat_per_byte = feerate.Value.ToString()
            )
        let! sendCoinsResp = Async.AwaitTask (client.SwaggerClient.SendCoinsAsync sendCoinsReq)
        return TxId <| uint256 sendCoinsResp.Txid
    }

    member this.ConnectTo (nodeId: NodeId) (ipEndPoint: IPEndPoint): Async<unit> =
        let client = this.Client()
        let nodeInfo = NodeInfo (nodeId.Value, ipEndPoint.Address.ToString(), ipEndPoint.Port)
        (Async.AwaitTask: Task -> Async<unit>) <| (client :> ILightningClient).ConnectTo nodeInfo

    member this.OpenChannel (nodeId: NodeId)
                            (ipEndPoint: IPEndPoint)
                            (amount: Money)
                            (feeRate: FeeRatePerKw)
                                : Async<Result<unit, OpenChannelResult>> = async {
        let client = this.Client()
        let nodeInfo = NodeInfo (nodeId.Value, ipEndPoint.Address.ToString(), ipEndPoint.Port)
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

[<TestFixture>]
type LN() =
    do Config.SetRunModeToTesting()

    [<Test>]
    member __.``can open channel with LND``() = Async.RunSynchronously <| async {
        let geewalletPassword = Path.GetRandomFileName()

        do!
            let privateKeyByteLength = 32
            let privateKeyBytes: array<byte> = Array.zeroCreate privateKeyByteLength
            System.Random().NextBytes privateKeyBytes
            Account.CreateAllAccounts privateKeyBytes geewalletPassword

        let rec waitForBalance(btcAccount: NormalUtxoAccount) (minAmount: Money): Async<Money> = async {
            let! cachedBalance = Account.GetShowableBalance btcAccount ServerSelectionMode.Analysis None
            match cachedBalance with
            | Fresh amount when amount < minAmount.ToDecimal MoneyUnit.BTC ->
                do! Async.Sleep 500
                return! waitForBalance btcAccount minAmount
            | NotFresh _ ->
                do! Async.Sleep 500
                return! waitForBalance btcAccount minAmount
            | Fresh amount -> return Money(amount, MoneyUnit.BTC)
        }

        use bitcoind = Bitcoind.Start()
        use _electrumServer = ElectrumServer.Start bitcoind
        use! lnd = Lnd.Start bitcoind

        let! address = lnd.GetDepositAddress()
        let blocksMinedToLnd = BlockHeightOffset32 1u
        bitcoind.GenerateBlocks blocksMinedToLnd address

        let account: IAccount = Account.GetAllActiveAccounts() |> Seq.filter (fun x -> x.Currency = Currency.BTC) |> Seq.head
        let geewalletAddress = BitcoinScriptAddress(account.PublicAddress, Network.RegTest)
        let btcAccount = account :?> NormalUtxoAccount

        // Geewallet cannot use these outputs, even though they are encumbered with an output
        // script from its wallet. This is because they come from coinbase. Coinbase outputs are
        // the source of all bitcoin, and as of May 2020, Geewallet does not detect coins
        // received straight from coinbase. In practice, this doesn't matter, since miners
        // do not use Geewallet. If the coins were to be detected by geewallet,
        // this test would still work. This comment is just here to avoid confusion.
        let maturityDurationInNumberOfBlocks = BlockHeightOffset32 (uint32 NBitcoin.Consensus.RegTest.CoinbaseMaturity)
        bitcoind.GenerateBlocks maturityDurationInNumberOfBlocks geewalletAddress

        // We confirm the one block mined to LND, by waiting for LND to see the chain
        // at a height which has that block matured. The height at which the block will
        // be matured is 100 on regtest. Since we initialally mined one block for LND,
        // this will wait until the block height of LND reaches 1 (initial blocks mined)
        // plus 100 blocks (coinbase maturity). This test has been parameterized
        // to use the constants defined in NBitcoin, but you have to keep in mind that
        // the coinbase maturity may be defined differently in other coins.
        do! lnd.WaitForBlockHeight (BlockHeight.Zero + blocksMinedToLnd + maturityDurationInNumberOfBlocks)
        do! lnd.WaitForBalance (Money(50UL, MoneyUnit.BTC))

        // fund geewallet
        let geewalletAccountAmount = Money (25m, MoneyUnit.BTC)
        let feeRate = FeeRatePerKw 2500u
        let! _txid = lnd.SendCoins geewalletAccountAmount geewalletAddress feeRate

        // wait for lnd's transaction to appear in mempool
        while bitcoind.GetTxIdsInMempool().Length = 0 do
            Thread.Sleep 500

        // We want to make sure Geewallet consideres the money received.
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
        bitcoind.GenerateBlocks consideredConfirmedAmountOfBlocksPlusOne geewalletAddress

        let channelStore = ChannelStore btcAccount
        let node =
            let geewalletLightningBindAddress = IPEndPoint (IPAddress.Parse "127.0.0.1", 9735)
            Connection.Start channelStore geewalletPassword geewalletLightningBindAddress

        let! lndEndPoint = lnd.GetEndPoint()
        let! transferAmount = async {
            let amount = Money(0.0002m, MoneyUnit.BTC)
            let! accountBalance = waitForBalance btcAccount amount
            return TransferAmount (amount.ToDecimal MoneyUnit.BTC, accountBalance.ToDecimal MoneyUnit.BTC, Currency.BTC)
        }
        let! metadata = ChannelManager.EstimateChannelOpeningFee btcAccount transferAmount
        let! pendingChannelRes =
            Lightning.Network.OpenChannel
                node
                lndEndPoint
                transferAmount
                metadata
                geewalletPassword
        let pendingChannel = UnwrapResult pendingChannelRes "OpenChannel failed"
        let minimumDepth = (pendingChannel :> IChannelToBeOpened).ConfirmationsRequired
        let channelId = (pendingChannel :> IChannelToBeOpened).ChannelId
        let! fundingTxIdRes = pendingChannel.Accept()
        let _fundingTxId = UnwrapResult fundingTxIdRes "pendingChannel.Accept failed"
        bitcoind.GenerateBlocks (BlockHeightOffset32 minimumDepth) geewalletAddress

        do!
            let channelInfo = channelStore.ChannelInfo channelId
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

        let! lockFundingRes = Lightning.Network.LockChannelFunding node channelId
        UnwrapResult lockFundingRes "LockChannelFunding failed"

        let channelInfo = channelStore.ChannelInfo channelId
        match channelInfo.Status with
        | ChannelStatus.Active -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

        return ()
    }

