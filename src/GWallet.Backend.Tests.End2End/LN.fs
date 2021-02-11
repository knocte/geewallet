namespace GWallet.Backend.Tests.End2End

open NUnit.Framework

open System.Net
open System.Threading

open NBitcoin
open DotNetLightning.Utils

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.UtxoCoin.Lightning
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks

[<TestFixture>]
type LN() =
    do Config.SetRunModeToTesting()

    let walletToWalletTestPayment0Amount = Money (0.01m, MoneyUnit.BTC)
    let walletToWalletTestPayment1Amount = Money (0.015m, MoneyUnit.BTC)

    let fundeeAccountsPrivateKey =
        // Note: The key needs to be hard-coded, as opposed to randomly
        // generated, since it is used in two separate processes and must be
        // the same in each process.
        new Key (uint256.Parse("9d1ee30acb68716ed5f4e25b3c052c6078f1813f45d33a47e46615bfd05fa6fe").ToBytes())
    let fundeeNodePubKey =
        let extKey = WalletInstance.AccountPrivateKeyToNodeSecret fundeeAccountsPrivateKey
        extKey.PrivateKey.PubKey
    let fundeeLightningIPEndpoint = IPEndPoint (IPAddress.Parse "127.0.0.1", 9735)
    let fundeeNodeEndpoint =
        NodeEndPoint.Parse
            Currency.BTC
            (SPrintF3
                "%s@%s:%d"
                (fundeeNodePubKey.ToHex())
                (fundeeLightningIPEndpoint.Address.ToString())
                fundeeLightningIPEndpoint.Port
            )

    [<Category "G2GChannelOpeningFunder">]
    [<Test>]
    member __.``can open channel with geewallet (funder)``() = Async.RunSynchronously <| async {
        use! walletInstance = WalletInstance.New None None
        use bitcoind = Bitcoind.Start()
        use _electrumServer = ElectrumServer.Start bitcoind
        use! lnd = Lnd.Start bitcoind

        // As explained in the other test, geewallet cannot use coinbase outputs.
        // To work around that we mine a block to a LND instance and afterwards tell
        // it to send funds to the funder geewallet instance
        let! address = lnd.GetDepositAddress()
        let blocksMinedToLnd = BlockHeightOffset32 1u
        bitcoind.GenerateBlocks blocksMinedToLnd address

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
                fundeeNodeEndpoint
                transferAmount
                metadata
                walletInstance.Password
        let pendingChannel = UnwrapResult pendingChannelRes "OpenChannel failed"
        let minimumDepth = (pendingChannel :> IChannelToBeOpened).ConfirmationsRequired
        let channelId = (pendingChannel :> IChannelToBeOpened).ChannelId
        let! fundingTxIdRes = pendingChannel.Accept()
        let _fundingTxId = UnwrapResult fundingTxIdRes "pendingChannel.Accept failed"
        bitcoind.GenerateBlocks (BlockHeightOffset32 minimumDepth) walletInstance.Address

        do! walletInstance.WaitForFundingConfirmed channelId

        let! lockFundingRes = Lightning.Network.ConnectLockChannelFunding walletInstance.NodeServer.NodeClient channelId
        UnwrapResult lockFundingRes "LockChannelFunding failed"

        let channelInfo = walletInstance.ChannelStore.ChannelInfo channelId
        match channelInfo.Status with
        | ChannelStatus.Active -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

        return ()
    }

    [<Category "G2GChannelOpeningFundee">]
    [<Test>]
    member __.``can open channel with geewallet (fundee)``() = Async.RunSynchronously <| async {
        use! walletInstance = WalletInstance.New (Some fundeeLightningIPEndpoint) (Some fundeeAccountsPrivateKey)
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

        return ()
    }

    [<Category "G2GMonoHopUnidirectionalPaymentsFunder">]
    [<Test>]
    member __.``can send monohop payments (funder)``() = Async.RunSynchronously <| async {
        use! walletInstance = WalletInstance.New None None
        use bitcoind = Bitcoind.Start()
        use _electrumServer = ElectrumServer.Start bitcoind
        use! lnd = Lnd.Start bitcoind

        // As explained in the other test, geewallet cannot use coinbase outputs.
        // To work around that we mine a block to a LND instance and afterwards tell
        // it to send funds to the funder geewallet instance
        let! address = lnd.GetDepositAddress()
        let blocksMinedToLnd = BlockHeightOffset32 1u
        bitcoind.GenerateBlocks blocksMinedToLnd address

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
                fundeeNodeEndpoint
                transferAmount
                metadata
                walletInstance.Password
        let pendingChannel = UnwrapResult pendingChannelRes "OpenChannel failed"
        let minimumDepth = (pendingChannel :> IChannelToBeOpened).ConfirmationsRequired
        let channelId = (pendingChannel :> IChannelToBeOpened).ChannelId
        let! fundingTxIdRes = pendingChannel.Accept()
        let _fundingTxId = UnwrapResult fundingTxIdRes "pendingChannel.Accept failed"
        bitcoind.GenerateBlocks (BlockHeightOffset32 minimumDepth) walletInstance.Address

        do! walletInstance.WaitForFundingConfirmed channelId

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
                TransferAmount (walletToWalletTestPayment0Amount.ToDecimal MoneyUnit.BTC, accountBalance.ToDecimal MoneyUnit.BTC, Currency.BTC)
            Lightning.Network.SendMonoHopPayment
                walletInstance.NodeServer.NodeClient
                channelId
                transferAmount
        UnwrapResult sendMonoHopPayment0Res "SendMonoHopPayment failed"

        let channelInfoAfterPayment0 = walletInstance.ChannelStore.ChannelInfo channelId
        match channelInfo.Status with
        | ChannelStatus.Active -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

        if Money(channelInfoAfterPayment0.Balance, MoneyUnit.BTC) <> fundingAmount - walletToWalletTestPayment0Amount then
            failwith "incorrect balance after payment 0"

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

        if Money(channelInfoAfterPayment1.Balance, MoneyUnit.BTC) <> fundingAmount - walletToWalletTestPayment0Amount - walletToWalletTestPayment1Amount then
            failwith "incorrect balance after payment 1"

        return ()
    }


    [<Category "G2GMonoHopUnidirectionalPaymentsFundee">]
    [<Test>]
    member __.``can receive mono-hop unidirectional payments, with geewallet (fundee)``() = Async.RunSynchronously <| async {
        use! walletInstance = WalletInstance.New (Some fundeeLightningIPEndpoint) (Some fundeeAccountsPrivateKey)
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

        if Money(channelInfoAfterPayment0.Balance, MoneyUnit.BTC) <> walletToWalletTestPayment0Amount then
            failwith "incorrect balance after receiving payment 0"

        let! receiveMonoHopPaymentRes =
            Lightning.Network.ReceiveMonoHopPayment walletInstance.NodeServer channelId
        UnwrapResult receiveMonoHopPaymentRes "ReceiveMonoHopPayment failed"

        let channelInfoAfterPayment1 = walletInstance.ChannelStore.ChannelInfo channelId
        match channelInfo.Status with
        | ChannelStatus.Active -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

        if Money(channelInfoAfterPayment1.Balance, MoneyUnit.BTC) <> walletToWalletTestPayment0Amount + walletToWalletTestPayment1Amount then
            failwith "incorrect balance after receiving payment 1"

        return ()
    }

    [<Test>]
    member __.``can open channel with LND``() = Async.RunSynchronously <| async {
        use! walletInstance = WalletInstance.New None None

        use bitcoind = Bitcoind.Start()
        use _electrumServer = ElectrumServer.Start bitcoind
        use! lnd = Lnd.Start bitcoind

        let! address = lnd.GetDepositAddress()
        let blocksMinedToLnd = BlockHeightOffset32 1u
        bitcoind.GenerateBlocks blocksMinedToLnd address

        // Geewallet cannot use these outputs, even though they are encumbered with an output
        // script from its wallet. This is because they come from coinbase. Coinbase outputs are
        // the source of all bitcoin, and as of May 2020, Geewallet does not detect coins
        // received straight from coinbase. In practice, this doesn't matter, since miners
        // do not use Geewallet. If the coins were to be detected by geewallet,
        // this test would still work. This comment is just here to avoid confusion.
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

        let! lndEndPoint = lnd.GetEndPoint()
        let! transferAmount = async {
            let amount = Money(0.002m, MoneyUnit.BTC)
            let! accountBalance = walletInstance.WaitForBalance amount
            return TransferAmount (amount.ToDecimal MoneyUnit.BTC, accountBalance.ToDecimal MoneyUnit.BTC, Currency.BTC)
        }
        let! metadata = ChannelManager.EstimateChannelOpeningFee (walletInstance.Account :?> NormalUtxoAccount) transferAmount
        let! pendingChannelRes =
            Lightning.Network.OpenChannel
                walletInstance.NodeServer.NodeClient
                lndEndPoint
                transferAmount
                metadata
                walletInstance.Password
        let pendingChannel = UnwrapResult pendingChannelRes "OpenChannel failed"
        let minimumDepth = (pendingChannel :> IChannelToBeOpened).ConfirmationsRequired
        let channelId = (pendingChannel :> IChannelToBeOpened).ChannelId
        let! fundingTxIdRes = pendingChannel.Accept()
        let _fundingTxId = UnwrapResult fundingTxIdRes "pendingChannel.Accept failed"
        bitcoind.GenerateBlocks (BlockHeightOffset32 minimumDepth) walletInstance.Address

        do! walletInstance.WaitForFundingConfirmed channelId

        let! lockFundingRes = Lightning.Network.ConnectLockChannelFunding walletInstance.NodeServer.NodeClient channelId
        UnwrapResult lockFundingRes "LockChannelFunding failed"

        let channelInfo = walletInstance.ChannelStore.ChannelInfo channelId
        match channelInfo.Status with
        | ChannelStatus.Active -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

        return ()
    }

    [<Test>]
    member __.``can accept channel from LND``() = Async.RunSynchronously <| async {
        use! walletInstance = WalletInstance.New None None
        use bitcoind = Bitcoind.Start()
        use _electrumServer = ElectrumServer.Start bitcoind
        use! lnd = Lnd.Start bitcoind

        let! address = lnd.GetDepositAddress()
        let blocksMinedToLnd = BlockHeightOffset32 1u
        bitcoind.GenerateBlocks blocksMinedToLnd address

        // Geewallet cannot use these outputs, even though they are encumbered with an output
        // script from its wallet. This is because they come from coinbase. Coinbase outputs are
        // the source of all bitcoin, and as of May 2020, Geewallet does not detect coins
        // received straight from coinbase. In practice, this doesn't matter, since miners
        // do not use Geewallet. If the coins were to be detected by geewallet,
        // this test would still work. This comment is just here to avoid confusion.
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

        return ()
    }

