// because of the use of internal AcceptCloseChannel and ReceiveMonoHopPayment
#nowarn "44"

namespace GWallet.Backend.Tests.End2End

open System
open System.Threading

open NUnit.Framework
open NBitcoin
open DotNetLightning.Utils
open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.UtxoCoin.Lightning
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks

[<TestFixture>]
type LN() =
    do Config.SetRunModeToTesting()

    let walletToWalletTestPayment1Amount = Money (0.01m, MoneyUnit.BTC)
    let walletToWalletTestPayment2Amount = Money (0.015m, MoneyUnit.BTC)

    let Setup () =
        async {
            let! walletInstance = WalletInstance.New None None
            let! bitcoind = Bitcoind.Start()
            let! electrumServer = ElectrumServer.Start bitcoind
            let! lnd = Lnd.Start bitcoind

            return walletInstance, bitcoind, electrumServer, lnd
        }

    let TearDown walletInstance bitcoind electrumServer lnd =
        (walletInstance :> IDisposable).Dispose()
        (lnd :> IDisposable).Dispose()
        (electrumServer :> IDisposable).Dispose()
        (bitcoind :> IDisposable).Dispose()

    let OpenChannelWithFundee (nodeOpt: Option<NodeEndPoint>) =
        async {
            let! walletInstance, bitcoind, electrumServer, lnd = Setup()

            do! walletInstance.FundByMining bitcoind lnd

            let! lndEndPoint = lnd.GetEndPoint()

            let nodeEndPoint =
                match nodeOpt with
                | None -> lndEndPoint
                | Some node -> node

            let! channelId, fundingAmount = walletInstance.OpenChannelWithFundee bitcoind nodeEndPoint

            let channelInfoAfterOpening = walletInstance.ChannelStore.ChannelInfo channelId
            match channelInfoAfterOpening.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            return channelId, walletInstance, bitcoind, electrumServer, lnd, fundingAmount
        }

    let AcceptChannelFromLndFunder () =
        async {
            let! walletInstance, bitcoind, electrumServer, lnd = Setup()

            do! lnd.FundByMining bitcoind

            let acceptChannelTask = Lightning.Network.AcceptChannel walletInstance.NodeServer
            let openChannelTask = async {
                do! lnd.ConnectTo walletInstance.NodeEndPoint
                return!
                    lnd.OpenChannel
                        walletInstance.NodeEndPoint
                        (Money(0.002m, MoneyUnit.BTC))
                        (FeeRatePerKw 666u)
            }

            let! acceptChannelRes, openChannelRes = AsyncExtensions.MixedParallel2 acceptChannelTask openChannelTask
            let (channelId, _) = UnwrapResult acceptChannelRes "AcceptChannel failed"
            UnwrapResult openChannelRes "lnd.OpenChannel failed"

            // Wait for the funding transaction to appear in mempool
            while bitcoind.GetTxIdsInMempool().Length = 0 do
                Thread.Sleep 500

            // Mine blocks on top of the funding transaction to make it confirmed.
            let minimumDepth = BlockHeightOffset32 6u
            bitcoind.GenerateBlocks minimumDepth walletInstance.Address

            do! walletInstance.WaitForFundingConfirmed channelId

            let! lockFundingRes = Lightning.Network.AcceptLockChannelFunding walletInstance.NodeServer channelId
            UnwrapResult lockFundingRes "LockChannelFunding failed"

            let channelInfo = walletInstance.ChannelStore.ChannelInfo channelId
            match channelInfo.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            return channelId, walletInstance, bitcoind, electrumServer, lnd
        }

    let AcceptChannelFromGeewalletFunder () =
        async {
            let! walletInstance = WalletInstance.New (Some Config.FundeeLightningIPEndpoint) (Some Config.FundeeAccountsPrivateKey)
            let! pendingChannelRes =
                Lightning.Network.AcceptChannel
                    walletInstance.NodeServer

            let (channelId, _) = UnwrapResult pendingChannelRes "OpenChannel failed"

            let! lockFundingRes = Lightning.Network.AcceptLockChannelFunding walletInstance.NodeServer channelId
            UnwrapResult lockFundingRes "LockChannelFunding failed"

            let channelInfo = walletInstance.ChannelStore.ChannelInfo channelId
            match channelInfo.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            if Money(channelInfo.Balance, MoneyUnit.BTC) <> Money(0.0m, MoneyUnit.BTC) then
                failwith "incorrect balance after accepting channel"

            return walletInstance, channelId
        }

    let CloseChannel (walletInstance: WalletInstance) (bitcoind: Bitcoind) channelId =
        async {
            let! closeChannelRes = Lightning.Network.CloseChannel walletInstance.NodeServer.NodeClient channelId
            match closeChannelRes with
            | Ok _ -> ()
            | Error err -> failwith (SPrintF1 "error when closing channel: %s" err.Message)

            match (walletInstance.ChannelStore.ChannelInfo channelId).Status with
            | ChannelStatus.Closing -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Closing, got %A" status)

            // Mine 10 blocks to make sure closing tx is confirmed
            bitcoind.GenerateBlocks (BlockHeightOffset32 (uint32 10)) walletInstance.Address

            let rec waitForClosingTxConfirmed attempt = async {
                Infrastructure.LogDebug (SPrintF1 "Checking if closing tx is finished, attempt #%d" attempt)
                if attempt = 10 then
                    return Error "Closing tx not confirmed after maximum attempts"
                else
                    let! txIsConfirmed = Lightning.Network.CheckClosingFinished (walletInstance.ChannelStore.ChannelInfo channelId)
                    if txIsConfirmed then
                        return Ok ()
                    else
                        do! Async.Sleep 1000
                        return! waitForClosingTxConfirmed (attempt + 1)
            }

            let! closingTxConfirmedRes = waitForClosingTxConfirmed 0
            match closingTxConfirmedRes with
            | Ok _ -> ()
            | Error err -> failwith (SPrintF1 "error when waiting for closing tx to confirm: %s" err)
        }

    let SendMonoHopPayments (walletInstance: WalletInstance) channelId fundingAmount =
        async {
            let channelInfo = walletInstance.ChannelStore.ChannelInfo channelId

            let! sendMonoHopPayment1Res =
                let transferAmount =
                    let accountBalance = Money(channelInfo.SpendableBalance, MoneyUnit.BTC)
                    TransferAmount (walletToWalletTestPayment1Amount.ToDecimal MoneyUnit.BTC, accountBalance.ToDecimal MoneyUnit.BTC, Currency.BTC)
                Lightning.Network.SendMonoHopPayment
                    walletInstance.NodeServer.NodeClient
                    channelId
                    transferAmount
            UnwrapResult sendMonoHopPayment1Res "SendMonoHopPayment failed"

            let channelInfoAfterPayment1 = walletInstance.ChannelStore.ChannelInfo channelId
            match channelInfo.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            if Money(channelInfoAfterPayment1.Balance, MoneyUnit.BTC) <> fundingAmount - walletToWalletTestPayment1Amount then
                failwith "incorrect balance after payment 1"

            let! sendMonoHopPayment2Res =
                let transferAmount =
                    let accountBalance = Money(channelInfo.SpendableBalance, MoneyUnit.BTC)
                    TransferAmount (
                        walletToWalletTestPayment2Amount.ToDecimal MoneyUnit.BTC,
                        accountBalance.ToDecimal MoneyUnit.BTC, Currency.BTC
                    )
                Lightning.Network.SendMonoHopPayment
                    walletInstance.NodeServer.NodeClient
                    channelId
                    transferAmount
            UnwrapResult sendMonoHopPayment2Res "SendMonoHopPayment failed"

            let channelInfoAfterPayment2 = walletInstance.ChannelStore.ChannelInfo channelId
            match channelInfo.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            if Money(channelInfoAfterPayment2.Balance, MoneyUnit.BTC) <> fundingAmount - walletToWalletTestPayment1Amount - walletToWalletTestPayment2Amount then
                failwith "incorrect balance after payment 2"
        }

    let ReceiveMonoHopPayments (walletInstance: WalletInstance) channelId =
        async {
            let! receiveMonoHopPaymentRes =
                Lightning.Network.ReceiveMonoHopPayment walletInstance.NodeServer channelId
            UnwrapResult receiveMonoHopPaymentRes "ReceiveMonoHopPayment failed"

            let channelInfoAfterPayment1 = walletInstance.ChannelStore.ChannelInfo channelId
            match channelInfoAfterPayment1.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            if Money(channelInfoAfterPayment1.Balance, MoneyUnit.BTC) <> walletToWalletTestPayment1Amount then
                failwith "incorrect balance after receiving payment 1"

            let! receiveMonoHopPaymentRes =
                Lightning.Network.ReceiveMonoHopPayment walletInstance.NodeServer channelId
            UnwrapResult receiveMonoHopPaymentRes "ReceiveMonoHopPayment failed"

            let channelInfoAfterPayment2 = walletInstance.ChannelStore.ChannelInfo channelId
            match channelInfoAfterPayment2.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            if Money(channelInfoAfterPayment2.Balance, MoneyUnit.BTC) <> walletToWalletTestPayment1Amount + walletToWalletTestPayment2Amount then
                failwith "incorrect balance after receiving payment 2"
        }


    [<Category "G2G_ChannelOpening_Funder">]
    [<Test>]
    member __.``can open channel with geewallet (funder)``() = Async.RunSynchronously <| async {
        let! _channelId, walletInstance, bitcoind, electrumServer, lnd, _fundingAmount =
            OpenChannelWithFundee (Some Config.FundeeNodeEndpoint)

        TearDown walletInstance lnd electrumServer bitcoind
    }

    [<Category "G2G_ChannelOpening_Fundee">]
    [<Test>]
    member __.``can open channel with geewallet (fundee)``() = Async.RunSynchronously <| async {
        let! walletInstance, _channelId = AcceptChannelFromGeewalletFunder ()

        (walletInstance :> IDisposable).Dispose()
    }

    [<Category "G2G_ChannelClosingAfterJustOpening_Funder">]
    [<Test>]
    member __.``can close channel with geewallet (funder)``() = Async.RunSynchronously <| async {
        let! channelId, walletInstance, bitcoind, electrumServer, lnd, _fundingAmount =
            try
                OpenChannelWithFundee (Some Config.FundeeNodeEndpoint)
            with
            | ex ->
                Assert.Inconclusive (
                    sprintf
                        "Channel-closing inconclusive because Channel open failed, fix this first: %s"
                        (ex.ToString())
                )
                failwith "unreachable"

        do! CloseChannel walletInstance bitcoind channelId

        TearDown walletInstance lnd electrumServer bitcoind
    }

    [<Category "G2G_ChannelClosingAfterJustOpening_Fundee">]
    [<Test>]
    member __.``can close channel with geewallet (fundee)``() = Async.RunSynchronously <| async {
        let! walletInstance, channelId = AcceptChannelFromGeewalletFunder ()

        let! closeChannelRes = Lightning.Network.AcceptCloseChannel walletInstance.NodeServer channelId
        match closeChannelRes with
        | Ok _ -> ()
        | Error err -> failwith (SPrintF1 "failed to accept close channel: %A" err)
    }

    [<Category "G2G_MonoHopUnidirectionalPayments_Funder">]
    [<Test>]
    member __.``can send monohop payments (funder)``() = Async.RunSynchronously <| async {
        let! channelId, walletInstance, bitcoind, electrumServer, lnd, fundingAmount =
            try
                OpenChannelWithFundee (Some Config.FundeeNodeEndpoint)
            with
            | ex ->
                Assert.Inconclusive (
                    sprintf
                        "Monohop-sending inconclusive because Channel open failed, fix this first: %s"
                        (ex.ToString())
                )
                failwith "unreachable"

        do! SendMonoHopPayments walletInstance channelId fundingAmount

        TearDown walletInstance lnd electrumServer bitcoind
    }


    [<Category "G2G_MonoHopUnidirectionalPayments_Fundee">]
    [<Test>]
    member __.``can receive mono-hop unidirectional payments, with geewallet (fundee)``() = Async.RunSynchronously <| async {
        let! walletInstance, channelId = AcceptChannelFromGeewalletFunder ()

        do! ReceiveMonoHopPayments walletInstance channelId

        (walletInstance :> IDisposable).Dispose()
    }

    [<Category "G2G_ChannelClosingAfterSendingMonoHopPayments_Funder">]
    [<Test>]
    member __.``can close channel after sending monohop payments (funder)``() = Async.RunSynchronously <| async {
        let! channelId, walletInstance, bitcoind, electrumServer, lnd, fundingAmount =
            try
                OpenChannelWithFundee (Some Config.FundeeNodeEndpoint)
            with
            | ex ->
                Assert.Inconclusive (
                    sprintf
                        "Channel-closing inconclusive because Channel open failed, fix this first: %s"
                        (ex.ToString())
                )
                failwith "unreachable"

        try
            do! SendMonoHopPayments walletInstance channelId fundingAmount
        with
        | ex ->
            Assert.Inconclusive (
                sprintf
                    "Channel-closing inconclusive because sending of monohop payments failed, fix this first: %s"
                    (ex.ToString())
            )
            failwith "unreachable"

        do! CloseChannel walletInstance bitcoind channelId

        TearDown walletInstance lnd electrumServer bitcoind
    }


    [<Category "G2G_ChannelClosingAfterSendingMonoHopPayments_Fundee">]
    [<Test>]
    member __.``can close channel after receiving mono-hop unidirectional payments, with geewallet (fundee)``() = Async.RunSynchronously <| async {
        let! walletInstance, channelId = AcceptChannelFromGeewalletFunder ()

        do! ReceiveMonoHopPayments walletInstance channelId

        let! closeChannelRes = Lightning.Network.AcceptCloseChannel walletInstance.NodeServer channelId
        match closeChannelRes with
        | Ok _ -> ()
        | Error err -> failwith (SPrintF1 "failed to accept close channel: %A" err)

        (walletInstance :> IDisposable).Dispose()
    }

    [<Test>]
    member __.``can open channel with LND``() = Async.RunSynchronously <| async {
        let! _channelId, walletInstance, bitcoind, electrumServer, lnd, _fundingAmount = OpenChannelWithFundee None

        TearDown walletInstance lnd electrumServer bitcoind
    }

    [<Test>]
    member __.``can close channel with LND``() = Async.RunSynchronously <| async {
        let! channelId, walletInstance, bitcoind, electrumServer, lnd, _fundingAmount =
            try
                OpenChannelWithFundee None
            with
            | ex ->
                Assert.Inconclusive (
                    sprintf
                        "Channel-closing inconclusive because Channel open failed, fix this first: %s"
                        (ex.ToString())
                )
                failwith "unreachable"

        let! closeChannelRes = Lightning.Network.CloseChannel walletInstance.NodeServer.NodeClient channelId
        match closeChannelRes with
        | Ok _ -> ()
        | Error err -> failwith (SPrintF1 "error when closing channel: %s" err.Message)

        match (walletInstance.ChannelStore.ChannelInfo channelId).Status with
        | ChannelStatus.Closing -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Closing, got %A" status)

        // Mine 10 blocks to make sure closing tx is confirmed
        bitcoind.GenerateBlocks (BlockHeightOffset32 (uint32 10)) walletInstance.Address

        let rec waitForClosingTxConfirmed attempt = async {
            Infrastructure.LogDebug (SPrintF1 "Checking if closing tx is finished, attempt #%d" attempt)
            if attempt = 10 then
                return Error "Closing tx not confirmed after maximum attempts"
            else
                let! txIsConfirmed = Lightning.Network.CheckClosingFinished (walletInstance.ChannelStore.ChannelInfo channelId)
                if txIsConfirmed then
                    return Ok ()
                else
                    do! Async.Sleep 1000
                    return! waitForClosingTxConfirmed (attempt + 1)
        }

        let! closingTxConfirmedRes = waitForClosingTxConfirmed 0
        match closingTxConfirmedRes with
        | Ok _ -> ()
        | Error err -> failwith (SPrintF1 "error when waiting for closing tx to confirm: %s" err)

        TearDown walletInstance lnd electrumServer bitcoind
    }

    [<Test>]
    member __.``can accept channel from LND``() = Async.RunSynchronously <| async {
        let! _channelId, walletInstance, bitcoind, electrumServer, lnd = AcceptChannelFromLndFunder ()

        TearDown walletInstance lnd electrumServer bitcoind
    }

    [<Test>]
    member __.``can accept channel closure from LND``() = Async.RunSynchronously <| async {
        let! channelId, walletInstance, bitcoind, electrumServer, lnd =
            try
                AcceptChannelFromLndFunder ()
            with
            | ex ->
                Assert.Inconclusive (
                    sprintf
                        "Channel-closing inconclusive because Channel accept failed, fix this first: %s"
                        (ex.ToString())
                )
                failwith "unreachable"

        let channelInfo = walletInstance.ChannelStore.ChannelInfo channelId

        // wait for lnd to realise we're offline
        do! Async.Sleep 1000
        let fundingOutPoint =
            let fundingTxId = uint256(channelInfo.FundingTxId.ToString())
            let fundingOutPointIndex = channelInfo.FundingOutPointIndex
            OutPoint(fundingTxId, fundingOutPointIndex)
        let closeChannelTask = async {
            do! lnd.ConnectTo walletInstance.NodeEndPoint
            do! Async.Sleep 1000
            do! lnd.CloseChannel fundingOutPoint
            return ()
        }
        let awaitCloseTask = async {
            let rec receiveEvent () = async {
                let! receivedEvent = Lightning.Network.ReceiveLightningEvent walletInstance.NodeServer channelId
                match receivedEvent with
                | Error err ->
                    return Error (SPrintF1 "Failed to receive shutdown msg from LND: %A" err)
                | Ok event when event = IncomingChannelEvent.Shutdown ->
                    return Ok ()
                | _ -> return! receiveEvent ()
            }

            let! receiveEventRes = receiveEvent()
            UnwrapResult receiveEventRes "failed to accept close channel"

            // Wait for the closing transaction to appear in mempool
            while bitcoind.GetTxIdsInMempool().Length = 0 do
                Thread.Sleep 500

            // Mine blocks on top of the closing transaction to make it confirmed.
            let minimumDepth = BlockHeightOffset32 6u
            bitcoind.GenerateBlocks minimumDepth walletInstance.Address
            return ()
        }

        let! (), () = AsyncExtensions.MixedParallel2 closeChannelTask awaitCloseTask

        TearDown walletInstance lnd electrumServer bitcoind

        return ()
    }

    [<Category "G2G_ChannelForceClosing_Funder">]
    [<Ignore "not ready yet">]
    [<Test>]
    member __.``can send monohop payments and force-close channel (funder)``() = Async.RunSynchronously <| async {
        use! walletInstance = WalletInstance.New None None
        use! bitcoind = Bitcoind.Start()
        use! _electrumServer = ElectrumServer.Start bitcoind
        use! lnd = Lnd.Start bitcoind

        // As explained in the other test, geewallet cannot use coinbase outputs.
        // To work around that we mine a block to a LND instance and afterwards tell
        // it to send funds to the funder geewallet instance
        let! lndAddress = lnd.GetDepositAddress()
        let blocksMinedToLnd = BlockHeightOffset32 1u
        bitcoind.GenerateBlocks blocksMinedToLnd lndAddress

        let maturityDurationInNumberOfBlocks = BlockHeightOffset32 (uint32 NBitcoin.Consensus.RegTest.CoinbaseMaturity)
        bitcoind.GenerateBlocks maturityDurationInNumberOfBlocks walletInstance.Address

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
        let! _txid = lnd.SendCoins geewalletAccountAmount walletInstance.Address feeRate

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
        bitcoind.GenerateBlocks consideredConfirmedAmountOfBlocksPlusOne walletInstance.Address

        let fundingAmount = Money(0.1m, MoneyUnit.BTC)
        let! transferAmount = async {
            let! accountBalance = walletInstance.WaitForBalance fundingAmount
            return TransferAmount (fundingAmount.ToDecimal MoneyUnit.BTC, accountBalance.ToDecimal MoneyUnit.BTC, Currency.BTC)
        }
        let! metadata = ChannelManager.EstimateChannelOpeningFee (walletInstance.Account :?> NormalUtxoAccount) transferAmount
        let! pendingChannelRes =
            Lightning.Network.OpenChannel
                walletInstance.NodeServer.NodeClient
                Config.FundeeNodeEndpoint
                transferAmount
                metadata
                walletInstance.Password
        let pendingChannel = UnwrapResult pendingChannelRes "OpenChannel failed"
        let minimumDepth = (pendingChannel :> IChannelToBeOpened).ConfirmationsRequired
        let channelId = (pendingChannel :> IChannelToBeOpened).ChannelId
        let! fundingTxIdRes = pendingChannel.Accept()
        let _fundingTxId = UnwrapResult fundingTxIdRes "pendingChannel.Accept failed"
        bitcoind.GenerateBlocks (BlockHeightOffset32 minimumDepth) walletInstance.Address

        do!
            let channelInfo = walletInstance.ChannelStore.ChannelInfo channelId
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

        let! lockFundingRes = Lightning.Network.ConnectLockChannelFunding walletInstance.NodeServer.NodeClient channelId
        UnwrapResult lockFundingRes "LockChannelFunding failed"

        let channelInfo = walletInstance.ChannelStore.ChannelInfo channelId
        match channelInfo.Status with
        | ChannelStatus.Active -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

        if Money(channelInfo.Balance, MoneyUnit.BTC) <> fundingAmount then
            failwith "balance does not match funding amount"

        let! sendMonoHopPayment0Res =
            let transferAmount =
                let accountBalance = Money(channelInfo.SpendableBalance, MoneyUnit.BTC)
                TransferAmount (walletToWalletTestPayment1Amount.ToDecimal MoneyUnit.BTC, accountBalance.ToDecimal MoneyUnit.BTC, Currency.BTC)
            Lightning.Network.SendMonoHopPayment
                walletInstance.NodeServer.NodeClient
                channelId
                transferAmount
        UnwrapResult sendMonoHopPayment0Res "SendMonoHopPayment failed"

        let channelInfoAfterPayment0 = walletInstance.ChannelStore.ChannelInfo channelId
        match channelInfo.Status with
        | ChannelStatus.Active -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

        if Money(channelInfoAfterPayment0.Balance, MoneyUnit.BTC) <> fundingAmount - walletToWalletTestPayment1Amount then
            failwith "incorrect balance after payment 0"

        let! sendMonoHopPayment1Res =
            let transferAmount =
                let accountBalance = Money(channelInfo.SpendableBalance, MoneyUnit.BTC)
                TransferAmount (walletToWalletTestPayment2Amount.ToDecimal MoneyUnit.BTC, accountBalance.ToDecimal MoneyUnit.BTC, Currency.BTC)
            Lightning.Network.SendMonoHopPayment
                walletInstance.NodeServer.NodeClient
                channelId
                transferAmount
        UnwrapResult sendMonoHopPayment1Res "SendMonoHopPayment failed"

        let channelInfoAfterPayment1 = walletInstance.ChannelStore.ChannelInfo channelId
        match channelInfo.Status with
        | ChannelStatus.Active -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

        if Money(channelInfoAfterPayment1.Balance, MoneyUnit.BTC) <> fundingAmount - walletToWalletTestPayment1Amount - walletToWalletTestPayment2Amount then
            failwith "incorrect balance after payment 1"

        let! _forceCloseTxId = Lightning.Network.ForceCloseChannel walletInstance.NodeServer.NodeClient channelId

        let locallyForceClosedData =
            match (walletInstance.ChannelStore.ChannelInfo channelId).Status with
            | ChannelStatus.LocallyForceClosed locallyForceClosedData ->
                locallyForceClosedData
            | status -> failwith (SPrintF1 "unexpected channel status. Expected LocallyForceClosed, got %A" status)

        // wait for force-close transaction to appear in mempool
        while bitcoind.GetTxIdsInMempool().Length = 0 do
            do! Async.Sleep 500

        Infrastructure.LogDebug (SPrintF1 "the time lock is %i blocks" locallyForceClosedData.ToSelfDelay)

        let! balanceBeforeFundsReclaimed = walletInstance.GetBalance()

        // Mine the force-close tx into a block
        bitcoind.GenerateBlocks (BlockHeightOffset32 1u) lndAddress

        // Mine blocks to release time-lock
        bitcoind.GenerateBlocks
            (BlockHeightOffset32 (uint32 locallyForceClosedData.ToSelfDelay))
            lndAddress

        let! _spendingTxId =
            UtxoCoin.Account.BroadcastRawTransaction
                locallyForceClosedData.Currency
                locallyForceClosedData.SpendingTransactionString

        // wait for spending transaction to appear in mempool
        while bitcoind.GetTxIdsInMempool().Length = 0 do
            do! Async.Sleep 500

        // Mine the spending tx into a block
        bitcoind.GenerateBlocks (BlockHeightOffset32 1u) lndAddress

        Infrastructure.LogDebug "waiting for our wallet balance to increase"
        let! _balanceAfterFundsReclaimed =
            let amount = balanceBeforeFundsReclaimed + Money(1.0m, MoneyUnit.Satoshi)
            walletInstance.WaitForBalance amount

        return ()
    }

    [<Category "G2G_ChannelForceClosing_Fundee">]
    [<Ignore "not ready yet">]
    [<Test>]
    member __.``can receive monohop payments and force-close channel (fundee)``() = Async.RunSynchronously <| async {
        use! walletInstance = WalletInstance.New (Some Config.FundeeLightningIPEndpoint) (Some Config.FundeeAccountsPrivateKey)
        let! pendingChannelRes =
            Lightning.Network.AcceptChannel
                walletInstance.NodeServer

        let (channelId, _) = UnwrapResult pendingChannelRes "OpenChannel failed"

        let! lockFundingRes = Lightning.Network.AcceptLockChannelFunding walletInstance.NodeServer channelId
        UnwrapResult lockFundingRes "LockChannelFunding failed"

        let channelInfo = walletInstance.ChannelStore.ChannelInfo channelId
        match channelInfo.Status with
        | ChannelStatus.Active -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

        if Money(channelInfo.Balance, MoneyUnit.BTC) <> Money(0.0m, MoneyUnit.BTC) then
            failwith "incorrect balance after accepting channel"

        let! receiveMonoHopPaymentRes =
            Lightning.Network.ReceiveMonoHopPayment walletInstance.NodeServer channelId
        UnwrapResult receiveMonoHopPaymentRes "ReceiveMonoHopPayment failed"

        let channelInfoAfterPayment0 = walletInstance.ChannelStore.ChannelInfo channelId
        match channelInfo.Status with
        | ChannelStatus.Active -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

        if Money(channelInfoAfterPayment0.Balance, MoneyUnit.BTC) <> walletToWalletTestPayment1Amount then
            failwith "incorrect balance after receiving payment 0"

        let! receiveMonoHopPaymentRes =
            Lightning.Network.ReceiveMonoHopPayment walletInstance.NodeServer channelId
        UnwrapResult receiveMonoHopPaymentRes "ReceiveMonoHopPayment failed"

        let channelInfoAfterPayment1 = walletInstance.ChannelStore.ChannelInfo channelId
        match channelInfo.Status with
        | ChannelStatus.Active -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

        if Money(channelInfoAfterPayment1.Balance, MoneyUnit.BTC) <> walletToWalletTestPayment1Amount + walletToWalletTestPayment2Amount then
            failwith "incorrect balance after receiving payment 1"

        return ()
    }

