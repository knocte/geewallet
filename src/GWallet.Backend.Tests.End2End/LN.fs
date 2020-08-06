namespace GWallet.Backend.Tests

open Microsoft.FSharp.Linq.NullableOperators // For <>?

open Newtonsoft.Json // For JsonConvert

open NUnit.Framework

open GWallet.Backend

open BTCPayServer.Lightning
open BTCPayServer.Lightning.LND

open System
open System.IO // For File.WriteAllText
open System.Diagnostics // For Process
open System.Net // For IPAddress and IPEndPoint
open System.Text // For Encoding
open System.Threading // For AutoResetEvent and CancellationToken
open System.Threading.Tasks // For Task

open NBitcoin // For ExtKey

open GWallet.Backend.UtxoCoin // For NormalUtxoAccount
open GWallet.Backend.UtxoCoin.Lightning

[<TestFixture>]
type LN() =

    let WatchOutput (proc: Process) (msgToWaitFor: List<string>) (verbose: bool): List<AutoResetEvent> =
        // triggers the events returned corresponding to the trigger strings passed
        let events =
            [for _ in msgToWaitFor do
                yield new AutoResetEvent(false)]
        let zipped = List.zip events msgToWaitFor
        let outputHandler (_: obj) (args: DataReceivedEventArgs) =
            let matched (evt, msg) = if args.Data.Contains msg then Some evt else None
            for event in List.choose matched zipped do
                event.Set() |> ignore
            if verbose then
                printfn "process output: %s" args.Data
        proc.OutputDataReceived.AddHandler(DataReceivedEventHandler outputHandler)
        proc.ErrorDataReceived.AddHandler(DataReceivedEventHandler outputHandler)
        proc.Exited.AddHandler(EventHandler (fun _ _ -> printfn "Process %s died!" proc.ProcessName))
        printfn "Watching output of process %s with pid %i" proc.ProcessName proc.Id
        proc.BeginOutputReadLine()
        proc.BeginErrorReadLine()
        events

    let FindExecutableInPath (python: bool) (given: string): string =
        let enviromentPath = System.Environment.GetEnvironmentVariable "PATH"
        let pathSeparator = Path.PathSeparator
        let paths = enviromentPath.Split pathSeparator
        let isWin = Path.DirectorySeparatorChar = '\\'
        let extension = if python && isWin then ".py" else ".exe"
        let exeName = if isWin then given + extension else given
        let paths = [ for x in paths do yield Path.Combine(x, exeName) ]
        let matching = paths |> List.filter File.Exists
        match matching with
        | first :: _ -> first
        | _ -> failwithf "Couldn't find %s in path, tried %A, these paths matched: %A" given [ for x in paths do yield (File.Exists x, x) ] matching

    [<Test>]
    member __.``can open channel with LND``() =
#if DEBUG
        Config.BitcoinNet <- NBitcoin.Network.RegTest
#else
        failwith "Cannot run this test without RegTest capability."
#endif

        let bitcoinDataDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())

        let generateBlocks (amount: int) (address: string) =
            let generate =
                ProcessStartInfo (
                    UseShellExecute = false,
                    FileName = FindExecutableInPath false "bitcoin-cli",
                    Arguments = "-regtest -datadir=" + bitcoinDataDir + " generatetoaddress " + (amount.ToString()) + " " + address
                )
            let generateProcess = Process.Start generate
            generateProcess.WaitForExit ()

        let lndDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
        let electrumXDBDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())

        for dir in [bitcoinDataDir; lndDir; electrumXDBDir] do
            Directory.CreateDirectory dir |> ignore

        let confPath = Path.Combine(bitcoinDataDir, "bitcoin.conf")
        File.WriteAllText(confPath, "\
            txindex=1\n\
            printtoconsole=1\n\
            rpcuser=doggman\n\
            rpcpassword=donkey\n\
            rpcallowip=127.0.0.1\n\
            zmqpubrawblock=tcp://127.0.0.1:28332\n\
            zmqpubrawtx=tcp://127.0.0.1:28333\n\
            fallbackfee=0.00001\n\
            [regtest]\n\
            rpcbind=127.0.0.1\n\
            rpcport=18554")

        let bitcoindStartInfo =
            ProcessStartInfo (
                UseShellExecute = false,
                FileName = "bitcoind",
                Arguments = "-regtest -datadir=" + bitcoinDataDir,
                RedirectStandardOutput = true,
                RedirectStandardError = true
            )
        use bitcoindProc = Process.Start bitcoindStartInfo
        (WatchOutput bitcoindProc ["init message: Done loading"] false).[0].WaitOne() |> ignore

        let electrumXStartInfo =
            ProcessStartInfo (
                UseShellExecute = false,
                FileName = FindExecutableInPath true "electrumx_server",
                Arguments = "",
                RedirectStandardOutput = true,
                RedirectStandardError = true
            )
        let env = electrumXStartInfo.Environment
        env.["SERVICES"] <- "tcp://[::1]:50001"
        env.["COIN"] <- "BitcoinSegwit"
        env.["NET"] <- "regtest"
        env.["DAEMON_URL"] <- "http://doggman:donkey@127.0.0.1:18554"
        env.["DB_DIRECTORY"] <- electrumXDBDir
        use electrumXProc = Process.Start electrumXStartInfo
        (WatchOutput electrumXProc ["TCP server listening on [::1]:50001"] true).[0].WaitOne() |> ignore

        let lndStartInfo =
            ProcessStartInfo (
                UseShellExecute = false,
                FileName = FindExecutableInPath false "lnd",
                Arguments = "--bitcoin.active --bitcoin.regtest --bitcoin.node=bitcoind --bitcoind.rpcuser=doggman --bitcoind.rpcpass=donkey --bitcoind.zmqpubrawblock=tcp://127.0.0.1:28332 --bitcoind.zmqpubrawtx=tcp://127.0.0.1:28333 --bitcoind.rpchost=localhost:18554 --debuglevel=trace --listen=127.0.0.2 --restlisten=127.0.0.2:8080 --lnddir=" + lndDir,

                RedirectStandardOutput = true,
                RedirectStandardError = true
            )
        use lndProc = Process.Start lndStartInfo
        // the lnd9375Ready event will trigger only after we initialize the wallet
        let (restReady, lnd9735Ready) =
            match WatchOutput lndProc ["password gRPC proxy started at 127.0.0.2:8080"; "Server listening on 127.0.0.2:9735"] false with
            | [restReady; lnReady] -> (restReady, lnReady)
            | _ -> failwith "programmer error: in-length should equal out-length of WatchOutput"
        restReady.WaitOne() |> ignore

        let connectionString: string = "type=lnd-rest;server=https://127.0.0.2:8080;allowinsecure=true;macaroonfilepath=" + Path.Combine(lndDir, "data/chain/bitcoin/regtest/admin.macaroon")
        let factory: ILightningClientFactory = new LightningClientFactory(NBitcoin.Network.RegTest) :> ILightningClientFactory
        let lndClient = factory.Create connectionString :?> LndClient

        Async.RunSynchronously <| async {
            let! genSeedResp = Async.AwaitTask <| lndClient.SwaggerClient.GenSeedAsync(null, null)
            printfn "Generated LND mnemonic: %A" genSeedResp.Cipher_seed_mnemonic

            let initWalletReq =
                LnrpcInitWalletRequest (
                    Wallet_password = Encoding.ASCII.GetBytes "password",
                    Cipher_seed_mnemonic = genSeedResp.Cipher_seed_mnemonic
                )

            let! _ = Async.AwaitTask <| lndClient.SwaggerClient.InitWalletAsync initWalletReq
            ()
        }

        printfn "Waiting for lnd open 9735..."
        lnd9735Ready.WaitOne() |> ignore
        printfn "Done waiting for lnd!"

        // We need to connect again for the connection to feature the methods of an unlocked wallet
        let client: ILightningClient = factory.Create connectionString
        let lndClient = client :?> LndClient
        let lndAddress = Async.RunSynchronously <| Async.AwaitTask (client.GetDepositAddress ())

        // Mine a block for LND, so that it has money. The number should be small, so that the
        // test executes quickly. The number should be larger than zero so that LND has money.
        // One is the smallest number that is not zero.
        let blocksMinedToLnd = 1
        generateBlocks blocksMinedToLnd (lndAddress.ToString())

        let iAccount: IAccount = Account.GetAllActiveAccounts() |> Seq.filter (fun x -> x.Currency = Currency.BTC) |> Seq.head
        let btcAccount = iAccount :?> NormalUtxoAccount

        // Geewallet cannot use these outputs, even though they are encumbered with an output
        // script from its wallet. This is because they come from coinbase. Coinbase outputs are
        // the source of all bitcoin, and as of May 2020, Geewallet does not detect coins
        // received straight from coinbase. In practise, this doesn't matter, since miners
        // do not use Geewallet. If the coins were to be detected by geewallet,
        // this test would still work. This comment is just here to avoid confusion.
        let maturityDurationInNumberOfBlocks = NBitcoin.Consensus.RegTest.CoinbaseMaturity
        generateBlocks maturityDurationInNumberOfBlocks iAccount.PublicAddress

        let height () =
            let getInfo = Async.RunSynchronously <| Async.AwaitTask (lndClient.SwaggerClient.GetInfoAsync ())
            printfn "lnd getinfo: identity_pubkey=%s height=%A" getInfo.Identity_pubkey getInfo.Block_height
            getInfo.Block_height

        // We confirm the one block mined to LND, by waiting for LND to see the chain
        // at a height which has that block matured. The height at which the block will
        // be matured is 100 on regtest. Since we initialally mined one block for LND,
        // this will wait until the block height of LND reaches 1 (initial blocks mined)
        // plus 100 blocks (coinbase maturity). This test has been parameterized
        // to use the constants defined in NBitcoin, but you have to keep in mind that
        // the coinbase maturity may be defined differently in other coins.
        while int64 (blocksMinedToLnd + maturityDurationInNumberOfBlocks) <>? height ()  do
            Thread.Sleep 500

        let walletBalance () =
            let balance = Async.RunSynchronously <| Async.AwaitTask (lndClient.SwaggerClient.WalletBalanceAsync ())
            printfn "lnd walletbalance: confirmed_balance=%s" balance.Confirmed_balance
            uint64 balance.Confirmed_balance

        while walletBalance () = 0UL do
            Thread.Sleep 500

        // fund geewallet
        let twentyFiveBTC = Money (25m, MoneyUnit.BTC)
        let sendCoinsReq =
            LnrpcSendCoinsRequest (
                Addr = iAccount.PublicAddress,
                Amount = (twentyFiveBTC.ToUnit MoneyUnit.Satoshi).ToString(),
                Sat_per_byte = "100"
            )
        let sendCoinsResp = Async.RunSynchronously <| Async.AwaitTask (lndClient.SwaggerClient.SendCoinsAsync sendCoinsReq)
        printfn "lnd sendcoins: txid=%s" sendCoinsResp.Txid

        let mempool () =
            let startInfo =
                ProcessStartInfo (
                    UseShellExecute = false,
                    FileName = FindExecutableInPath false "bitcoin-cli",
                    Arguments = "-regtest -datadir=" + bitcoinDataDir + " getrawmempool",
                    RedirectStandardOutput = true
                )
            let cmd = Process.Start startInfo
            let output = cmd.StandardOutput.ReadToEnd ()
            cmd.WaitForExit()
            let txidList = JsonConvert.DeserializeObject<List<string>> output
            printfn "bitcoin-cli: getrawmempool: %A" txidList
            txidList

        // wait for lnd's transaction to appear in mempool
        while (mempool ()).Length = 0 do
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
        let consideredConfirmedAmountOfBlocksPlusOne = 7
        generateBlocks consideredConfirmedAmountOfBlocksPlusOne iAccount.PublicAddress

        let nodeSecret = ExtKey ()
        let endpoint = IPEndPoint (IPAddress.Parse "127.0.0.1", 9735)
        let transportListener = Lightning.TransportListener.Bind nodeSecret endpoint

        let nodeId = nodeSecret.GetPublicKey ()
        let nodeInfo = NodeInfo (nodeId, endpoint.Address.ToString(), endpoint.Port)

        let lndOpens = false

        let geewalletJob: Async<bool> =
            async {
               try
                   let! initMsgStreamPairResult = Lightning.MsgStream.AcceptFromTransportListener transportListener
                   let (init, msgStream) = Util.Unwrap initMsgStreamPairResult "AcceptFromTransportListener failed"
                   printfn "Geewallet: accepted connection"
                   let peerWrapper: Lightning.PeerWrapper = { Init = init; MsgStream = msgStream }
                   if lndOpens then
                       let! fundedChannel = Lightning.FundedChannel.AcceptChannel peerWrapper btcAccount
                       printfn "Geewallet: FundedChannel: %A" fundedChannel
                   else
                       printfn "Geewallet: using servers: %A" (Caching.Instance.GetServers Currency.BTC)
                       let haveBalance () =
                           async {
                               let! cachedBalance = Account.GetShowableBalance btcAccount ServerSelectionMode.Analysis None
                               printfn "Geewallet balance: %A" cachedBalance
                               return
                                   match cachedBalance with
                                   | Fresh x -> x <> 0m
                                   | NotFresh _ -> false
                           }
                       let cont = ref true
                       while !cont do
                           let! res = haveBalance()
                           if res then
                               cont := false
                           else
                               Thread.Sleep 500
                       let amount = 0.0002m
                       let transferAmount = TransferAmount (amount, amount, Currency.BTC)
                       // this destination is just arbitrary, we don't know the funding address yet
                       let! metadata = Account.EstimateFee btcAccount transferAmount iAccount.PublicAddress
                       let! outgoingUnfundedChannel =
                           Lightning.OutgoingUnfundedChannel.OpenChannel
                               peerWrapper
                               btcAccount
                               transferAmount
                               metadata
                               "Password1"
                       printfn "Geewallet: OpenChannel result: %A" outgoingUnfundedChannel
                   return true
               with
               | e -> printfn "Geewallet error: %A" e
                      return false
            }

        let lndJob: Async<bool> =
            async {
                try
                    do! (Async.AwaitTask: Task -> Async<unit>) <| client.ConnectTo nodeInfo
                    printfn "We requested that LND connect to Geewallet!"
                    if lndOpens then
                        let openChannelReq =
                            new OpenChannelRequest (
                                NodeInfo = nodeInfo,
                                ChannelAmount = Money 20000UL,
                                FeeRate = new FeeRate (Money 1UL, 1)
                            )

                        let! openChannelResp = Async.AwaitTask <| client.OpenChannel openChannelReq
                        printfn "LND: openChannel: %A" openChannelResp.Result
                    return true
                with
                | e -> printfn "LND error: %s" e.Message
                       return false
            }

        let results = Async.RunSynchronously <| FSharpUtil.AsyncExtensions.MixedParallel2 geewalletJob lndJob
        Assert.That (results, Is.EqualTo (true, true))
