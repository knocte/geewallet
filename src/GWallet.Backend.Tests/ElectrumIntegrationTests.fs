namespace GWallet.Backend.Tests

open System
open System.Linq

open NUnit.Framework

open GWallet.Backend
open GWallet.Backend.UtxoCoin

// TODO: move this to its own file
[<TestFixture>]
type ElectrumServerUnitTests() =

    [<Test>]
    member __.``filters electrum BTC servers``() =
        for electrumServer in ElectrumServerSeedList.DefaultBtcList do
            Assert.That (electrumServer.ServerInfo.ConnectionType.Encrypted, Is.EqualTo false,
                sprintf "BTC servers list should be filtered against only-TLS compatible servers, but %s was found"
                        electrumServer.ServerInfo.NetworkPath)

            Assert.That (electrumServer.ServerInfo.NetworkPath, Is.Not.StringEnding ".onion",
                sprintf "BTC servers list should be filtered against onion servers, but %s was found"
                        electrumServer.ServerInfo.NetworkPath)

    [<Test>]
    member __.``filters electrum LTC servers``() =
        for electrumServer in ElectrumServerSeedList.DefaultLtcList do
            Assert.That (electrumServer.ServerInfo.ConnectionType.Encrypted, Is.EqualTo false,
                sprintf "BTC servers list should be filtered against only-TLS compatible servers, but %s was found"
                        electrumServer.ServerInfo.NetworkPath)

            Assert.That (electrumServer.ServerInfo.NetworkPath, Is.Not.StringEnding ".onion",
                sprintf "BTC servers list should be filtered against onion servers, but %s was found"
                        electrumServer.ServerInfo.NetworkPath)

[<TestFixture>]
[<Ignore ("Seems we have general issues reaching electrum servers these days, probably related to DDOS attack on them")>]
type ElectrumIntegrationTests() =

    // probably a satoshi address because it was used in blockheight 2 and is unspent yet
    let SCRIPTHASH_OF_SATOSHI_ADDRESS =
        // funny that it almost begins with "1HoDL"
        UtxoCoin.Account.GetElectrumScriptHashFromPublicAddress Currency.BTC "1HLoD9E4SDFFPDiYfNYnkBLQ85Y51J3Zb1"


    // https://medium.com/@SatoshiLite/satoshilite-1e2dad89a017
    let SCRIPTHASH_OF_LTC_GENESIS_BLOCK_ADDRESS =
        UtxoCoin.Account.GetElectrumScriptHashFromPublicAddress Currency.LTC "Ler4HNAEfwYhBmGXcFP2Po1NpRUEiK8km2"

    let CheckServerIsReachable (electrumServer: ServerDetails)
                               (currency: Currency)
                               (query: Async<StratumClient>->Async<'T>)
                               (assertion: 'T->unit)
                               (maybeFilter: Option<ServerDetails -> bool>)
                               : Async<Option<ServerDetails>> = async {

        let innerCheck server =
            // this try-with block is similar to the one in UtxoCoinAccount, where it rethrows as
            // ElectrumServerDiscarded error, but here we catch 2 of the 3 errors that are caught there
            // because we want the server incompatibilities to show up here (even if GWallet clients bypass
            // them in order not to crash)
            try
                let stratumClient = ElectrumClient.StratumServer server
                let result = query stratumClient
                                  |> Async.RunSynchronously

                assertion result

                Console.WriteLine (sprintf "%A server %s is reachable" currency server.ServerInfo.NetworkPath)
                Some electrumServer
            with
            | :? CommunicationUnsuccessfulException as ex ->
                // to make sure this exception type is an abstract class
                Assert.That(ex.GetType(), Is.Not.EqualTo typeof<CommunicationUnsuccessfulException>)

                let exDescription = sprintf "%s: %s" (ex.GetType().Name) ex.Message

                Console.Error.WriteLine (sprintf "%s -> %A server %s is unreachable" exDescription
                                                                                     currency
                                                                                     server.ServerInfo.NetworkPath)
                None

        match maybeFilter with
        | Some filterFunc ->
            if (filterFunc electrumServer) then
                return innerCheck electrumServer
            else
                return None
        | _ ->
            return innerCheck electrumServer

        }

    let BalanceAssertion (balance: BlockchainScriptHashGetBalanceInnerResult) =
        // if these ancient addresses get withdrawals it would be interesting in the crypto space...
        // so let's make the test check a balance like this which is unlikely to change
        Assert.That(balance.Confirmed, Is.Not.LessThan 998292)

    let rec AtLeastNJobsWork(jobs: List<Async<Option<ServerDetails>>>) (minimumCountNeeded: uint32): unit =
        match jobs with
        | [] ->
            if minimumCountNeeded > 0u then
                Assert.Fail (sprintf "Not enough servers were reached. Required: %i" minimumCountNeeded)
        | head::tail ->
            match head |> Async.RunSynchronously with
            | None ->
                AtLeastNJobsWork tail minimumCountNeeded
            | Some _ ->
                if minimumCountNeeded = 0u then
                    ()
                else
                    let newCount = (minimumCountNeeded - 1u)
                    AtLeastNJobsWork tail newCount

    let TestElectrumServersConnections (electrumServers: seq<_>)
                                       currency
                                       (query: Async<StratumClient>->Async<'T>)
                                       (assertion: 'T->unit)
                                       (atLeast: uint32)
                                           =
        if not (electrumServers.Any()) then
            failwith "list received shouldn't be empty"
        let reachServerJobs = seq {
            for electrumServer in electrumServers do
                yield CheckServerIsReachable electrumServer currency query assertion None
        }
        AtLeastNJobsWork (reachServerJobs |> List.ofSeq)
                         // more than one
                         atLeast

    let CheckElectrumServersConnection a b c d =
        TestElectrumServersConnections a b c d 2u

    let rebelBtcServerHostnames =
        [
            "E-X.not.fyi"
            "currentlane.lovebitco.in"
            "electrum.qtornado.com"
            "dedi.jochen-hoenicke.de"
            "electrum-server.ninja"
            "electrum.eff.ro"
            (* mmm, seems like the culprit is not in the specific servers because this list keeps growing and growing...
            "electrum.leblancnet.us"
            "daedalus.bauerj.eu"
            "electrum2.villocq.com"
            *)
        ]

    let btcNonRebelServers =
        List.filter
            (fun server -> rebelBtcServerHostnames.All(fun rebel -> server.ServerInfo.NetworkPath <> rebel))
            ElectrumServerSeedList.DefaultBtcList

    let btcRebelServers =
        List.filter
            (fun server -> rebelBtcServerHostnames.Any(fun rebel -> server.ServerInfo.NetworkPath = rebel))
            ElectrumServerSeedList.DefaultBtcList

    let UtxosAssertion (utxos: array<BlockchainScriptHashListUnspentInnerResult>) =
        // if these ancient addresses get withdrawals it would be interesting in the crypto space...
        // so let's make the test check a balance like this which is unlikely to change
        Assert.That(utxos.Length, Is.GreaterThan 1)

    let TxAssertion (txResult: string): unit =
        Assert.That(txResult, Is.EqualTo "0100000000010a2d230e9a4d85ccb2d075c8b635a36932f7c2951dd9a9b5dcc19a81357e1751d10000000023220020feaca27c22958225633d66ae1adf9c8e97517f94535389926b1a4d06cee33aeaffffffffd50c3fb1619f74c90b4619991a99032deaa3264cc2f741d46eab4b1b0eacb6b90300000023220020b70fabd600861defaa0dcaa5d74a4437b26bc4fd22ea2153be625f37961c5b94ffffffffd50c3fb1619f74c90b4619991a99032deaa3264cc2f741d46eab4b1b0eacb6b90e0000002322002051f0bf0942800ee51eb59ad5ec9a4a17cae66aebd582b7ae1ffa8395de45aaa3ffffffff2127745033bae7efda0dcb71fd28279c33c8b4ec8b178a32d90645cbda9bcef30100000023220020b4c490606793614822f95f96916a066c1f176a1f843607f9407aa644cd9f57bdffffffffdd9a754624f5348cbc8abc18ad9916e329dd66dce29c438123a36019704a6a8fa7000000232200200fa4eeff73da7cbb7326ad57b7b9040b220e49f30b00f491f04b2fa0bc1d2804ffffffffb84d643e98a6bd4f18665aba605ec7b8a18a33b29a0b332526f08ea61c5fcacf0100000023220020c982fa3052329fae293097728e790ba58cd25618efabf0b9b6c1e7433a260078ffffffff44360b39302ba36e00cee4305a2e4800ab631d129b820c9f707b3604823dda0700000000232200203a4e283d1e0b48e056678642f6e5bfe8151db0658f4c6fbad859fe02a9d7b69fffffffffe0136c03a9f573efc88371d818930be81a016e9648bc047dac4f9ac7a93f83a200000000232200203edb040081ee8fceed689e31c79cb67e781d33e85758f903edcd70ef4b75001dffffffff55c7cbe3c6a4233fd9aaabcd53b9013a0a61f6be8d70e3e5e54fec631770aeca00000000232200207b8b7ef82e696d835b06574476d46a683aa32f90cd8c4c6f5471282d7e698c0dffffffffd106ced87c0d350c9a4cb8b1ba69bf16428092b4c06898b6f9280f9a7396ce0904010000232200200fa4eeff73da7cbb7326ad57b7b9040b220e49f30b00f491f04b2fa0bc1d2804ffffffff0cf8162d000000000017a9145131075257d8b8de8298e7c52891eb4b87823b93873000c9010000000017a9140c1b069ba6d0691325cf15acf6b9e7c67f57b4c487b0feea0b0000000017a914e9210a740da86bc09c57045d9c53d03457a0209d8730208e030000000017a914e682c1068784415413f3feefa467e39da8fef3a187300b341d0000000017a914021f420dd06de86c7e82400e391e9c0d26423d518784233556000000001976a91466e1877107b93295873d7c607a0104c60064d38f88ac60df4000000000001976a91470f73e2ce9ebeadfbaf871e9b41b8ec3e56e9a7f88ac700ebd000000000016001449eb23af00ad1fa0cf32c6b52d92521b8c48f90be8b48c030000000017a9143003f822030a84b19d6cef5edab891a1ac6510128760b571000000000017a9147db9abfc8fce4da737a7108a030506d20c11acff87704a36000000000017a9147c7cedb301723552d1181460ea32f150dace05e9871adc762d0000000017a9144ad47d293d97e4d5cdfcbbdd4c3aa1453a9d48a487040047304402201facc7a954a6451b76a7e8c63b155280f5de9cbd48da6277cb40b26ace4e96de02203ccdf68c3bd917c6d5e85c642fc96824f380db7dbc726f02c52291de07226a350148304502210081622e0cda8b179dab0eeea68523f3cb1acdc98aaa84b860a7c1630a8073524d0220448044dca6a59267bb25bab67e4f827c91edc1f6c9675b2e1d293c7f4bf3e5c70169522102f72066131d60001b7d7abf39c6300aeb9f8cef16cb4fb9f238ae815111a73c49210285b875ddea788d7dae5b9d4d0a3992c65396c4c9d2fc710d4de74a21d8febbea210269a6a07f1168b5e2ae32844a8df637b23acf6a41f930e67b03b2b712dff6603453ae04004730440220750789f6ff389423fc6c2929341cdc55208798550ed722b718879617f15bf267022068f30b2c7d17c064e7e18856208b364bf53f0e9e1684a1165dd6f5ca94d98a5d01473044022059e9639cd64fe0c25c49a81993871aea9b5be81cf50d3507a685d84b5d2e153702205f4fb28081096f24f00686d23819d75f3c7f288dec7f9622d38b8d2172a63f710169522102952139ccb86b16383b29039bf76febbbfb1c41c15ec631c1b13f1424f7680a9321021c44aa55510b353e397f9f2b3e7dee36282f1421651fef96c31ba612c11627462103bbf6336d9f021ae4ee642af75156b644195dbe23a8de5cb62600b58e139d242653ae0400473044022070c844b19f917c16fc644957bafb3308b2cb244cdbce48ab763302d290272ef002204f6ca015707a9ffcbc3bee63c66a035430a7988495ce2f9c1d8e22f81c9ab0890147304402200ece13be1073c262d5a86c8078ce16fe6f2fa456665e6284eddd42b42006f74e022074fa8226a4ca112483736a9f0b47800b99197ffc8df3bcce6c3bef268a2a526a0169522102216a2779be169379d783d25180040e079da9193770e63bd5de8dc06c5834c0bd2102da354fb8873567ae83fef73a4fa783448a9b481e7f69619a20983149b1f19b402102e64c3c1e222b9cbdfb7f70b5bb011b89e0642eb7cfc94f2750948aeb54f8046553ae04004730440220431cde3ecb90c38c7f1bbafb4a417a8183e314eecc8a374fc67d5847b6822fd102201ff9e2ecc57bcdfb69ba1a9f78d132d7123b37e6838c7e77e37278b6269384e301483045022100df2ba7d95b973d30871a90f35757ad13b61c9f2deb8e36b675d19de4d5dfd12f02203f3b5dadd71304080a63c4031f4a6d7b30f3f742fc44e392a69cca947159d0890169522103630ae85a979ee329fa729b361a114254da4e871bce8629e39c26a26fb6ab85e82102d566976b3c356c2062a75e938ee538edef5ced6f6f8ca827c3b004b6c5f256a42102f5de580abf9fd69275cfc105ba165664d1d79b06f3aae1f512d778624c9ef8cb53ae0400473044022064b121d06f145acd2e84cf6ae73c6c8ba8119ac9450a93c9a8fcb882c98a7bd102206a5e8ff6b4b16b212d3c1cc92864ef9b2c804bc26f268399799a36414664923c0147304402200a99894da1fad45c7d62aceb0aa9d7a05f4304e4498327d8827f308ed8e70f43022034f68f701c3bb2afedc69ab7ba402a0d2277ef530fd89376836bb55151cf50540169522103ab69777a57f9cce9f3eea8079a2a1c89c07f9068022bf80686242efa1b60bbe021023718e1ca5f2f6e38841b627a8fa5c9a66f767d6a05367508cea960f21bb2feea2102e56e70e1cfac75b661131ac924aee909920d948ffb08f879bfa5ea1d3fecb9d353ae04004830450221008244512d55c6841c77a4d126fa5a33c92c247bf61d47c7dca0a7256ae8212e5f022019838e0aa334efa4a590b7398f3fdf6c2ef2a3d00f3856814ed04a9a0e56e54c01483045022100a8972a7a9a6852b0f96de6fa0fba67748551591ff9bec42a0df7d1f0a03392670220254d76e53a9c777aedbd2937c2f45324ff88a3ea15103bf9e325bd336d6cb2f501695221027d0f541fdf168131c9003d824c486062a9b6869f000cd65639a76255d05c3ee72103e86ad41675d5fcb7ce4d4b60e8f2ad122f873239d514bf1bceca49b74536a8aa2102805e8f635752cf9a42c38bb80f224b3212882e6c3c50bebba52eb7ab0935cae653ae040047304402202c71af53522c21e981863a336fd8b98394a981dbf8ac4a75c7b4f48c924faf56022057ef76dfcb327d39b4bc79ca5d3d604522ecfae6ee434f943ea714cf5ba0593301473044022068282d81f9968ef31db541e9b72aefff62aefd889e12c97f61e41e20d57d715d02202f54462beb8c4a462583c4917c3d937c036c5c5983c84c5be132cb9bef8d908c0169522102f116a19e2900371ae7f4be5523d9845654dedbce090e2fa30e816732d5d8251d2102d6f4005dfc28cb3c0def8c1f52f2d23d486dfbfa3ffa43c94a4b3d43c920c93e2103c6fac560735d78d95cd63652675e643c8e0433002679cd032143d8411dba6b1153ae0400483045022100a6bdc5206380571da68cde53f1ef19dca58c4d2d9cbc4220fe7e12e70fc9b0bd022011cad80c2bf83f85c111d5d3891f634a393c7ca57513ff2b5813e87bc85ae50501473044022100f598f696af768e2290f46873c3873876659fe60fe73c0068136e746a8f3327ef021f1b9de6928c8cf1245605ae705d7d1304efbbd385d4b77a81ab66bf8dda6a9401695221038acc7e5bd34e1198341f6c5561eba4580ce930ebfd78e9705fa51aaa34b8035b2103a40b87c3a637f4ab9b4cfadcf0537bfb71753b4f070e896eeeb0615bb21024f12103dbbc3ab658aef4b292042c3e99de6affcf573249a40bd6c225e4d84168bd01b253ae0400473044022041661469f59e7a72a9e1835323397fd4920e3a234c5c346afe4974c589ae25ee02207fe4b1c42b13dec0e1372446db604f8bda2be95c9bf0aa2c26dad8530ad11170014730440220417ce3b947dc37368af439c0afd347bcbbfed55b769e29bed1947079671197bf022079747806204ce6828a8d6b34703781f049124c39f8385fb1d1a880d9325e98850169522102aee5147d14dc3aa1da07594703864035112c1ce140ea7955599e9c3b911d65bc21022942c8ffc8271de9e763a3299bfc919b4077455c010aec920abd085df6ab73a621022c3f85c185fd927ed3e81f02ccaae1d45720c0929a0d1a6e9accd0c8d04342ef53ae040047304402206710fd815cbc1aaefae6fbc4b603f4f8b96516ab9f5dd69f371692ff0c2bfd34022040c6aebb39386aff976bb468b0c71e350a378b5a3847ccc9947ee522bb687c4e0147304402202cf317b968ed838f19634f5ea56ec0a521bbefeffe7a4c9498899d2b23fff4d5022009610c7bbd95f7cb212ee07299edfbb56c6ac8bf796b14ae9f2f64d5a08b79e30169522103ab69777a57f9cce9f3eea8079a2a1c89c07f9068022bf80686242efa1b60bbe021023718e1ca5f2f6e38841b627a8fa5c9a66f767d6a05367508cea960f21bb2feea2102e56e70e1cfac75b661131ac924aee909920d948ffb08f879bfa5ea1d3fecb9d353ae00000000")

    let GetScriptHash (currency: Currency) =
        match currency with
        | Currency.BTC -> SCRIPTHASH_OF_SATOSHI_ADDRESS
        | Currency.LTC -> SCRIPTHASH_OF_LTC_GENESIS_BLOCK_ADDRESS
        | _ -> failwith "Tests not ready for this currency"

    [<Test>]
    member __.``can connect (just check balance) to some electrum BTC servers``() =
        let currency = Currency.BTC
        let argument = GetScriptHash currency
        CheckElectrumServersConnection ElectrumServerSeedList.DefaultBtcList currency
                                       (ElectrumClient.GetBalance argument) BalanceAssertion

    [<Test>]
    member __.``can connect (just check balance) to some electrum LTC servers``() =
        let currency = Currency.LTC
        let argument = GetScriptHash currency
        CheckElectrumServersConnection ElectrumServerSeedList.DefaultLtcList currency
                                       (ElectrumClient.GetBalance argument) BalanceAssertion

    [<Test>]
    member __.``can get list UTXOs of an address from some electrum BTC servers``() =
        let currency = Currency.BTC
        let argument = GetScriptHash currency
        CheckElectrumServersConnection btcNonRebelServers currency
                                       (ElectrumClient.GetUnspentTransactionOutputs argument) UtxosAssertion

    [<Test>]
    // to make sure the workaround for https://github.com/nblockchain/JsonRpcSharp/issues/9 works
    member __.``should not get empty/null response from electrum BTC servers I``() =
        let currency = Currency.BTC

        // some random existing transaction
        let argument = "2f309ef555110ab4e9c920faa2d43e64f195aa027e80ec28e1d243bd8929a2fc"

        CheckElectrumServersConnection btcNonRebelServers currency
                                       (ElectrumClient.GetBlockchainTransaction argument) TxAssertion

    [<Test>]
    // to make sure the workaround for https://github.com/nblockchain/JsonRpcSharp/issues/9 works
    member __.``should not get empty/null response from electrum BTC servers II``() =
        let currency = Currency.BTC

        // some random existing transaction
        let argument = "2f309ef555110ab4e9c920faa2d43e64f195aa027e80ec28e1d243bd8929a2fc"

        CheckElectrumServersConnection btcRebelServers currency
                                       (ElectrumClient.GetBlockchainTransaction argument) TxAssertion

