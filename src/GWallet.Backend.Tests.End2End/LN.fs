// because of the use of internal AcceptCloseChannel and ReceiveMonoHopPayment
#nowarn "44"

namespace GWallet.Backend.Tests.End2End

open System
open System.Threading

open NUnit.Framework
open NBitcoin
open DotNetLightning.Payment
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

    let TearDown walletInstance bitcoind electrumServer lnd =
        (walletInstance :> IDisposable).Dispose()
        (lnd :> IDisposable).Dispose()
        (electrumServer :> IDisposable).Dispose()
        (bitcoind :> IDisposable).Dispose()

    let OpenChannelWithFundee (nodeOpt: Option<NodeEndPoint>) =
        async {
            let! clientWallet = ClientWalletInstance.New None
            let! bitcoind = Bitcoind.Start()
            let! electrumServer = ElectrumServer.Start bitcoind
            let! lnd = Lnd.Start bitcoind

            do! clientWallet.FundByMining bitcoind lnd

            let! lndEndPoint = lnd.GetEndPoint()

            let nodeEndPoint =
                match nodeOpt with
                | None -> lndEndPoint
                | Some node -> node

            let! channelId, fundingAmount = clientWallet.OpenChannelWithFundee bitcoind nodeEndPoint

            let channelInfoAfterOpening = clientWallet.ChannelStore.ChannelInfo channelId
            match channelInfoAfterOpening.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            return channelId, clientWallet, bitcoind, electrumServer, lnd, fundingAmount
        }

    let AcceptChannelFromLndFunder () =
        async {
            let! serverWallet = ServerWalletInstance.New Config.FundeeLightningIPEndpoint None
            let! bitcoind = Bitcoind.Start()
            let! electrumServer = ElectrumServer.Start bitcoind
            let! lnd = Lnd.Start bitcoind

            do! lnd.FundByMining bitcoind

            let! feeRate = ElectrumServer.EstimateFeeRate()
            let acceptChannelTask = Lightning.Network.AcceptChannel serverWallet.NodeServer
            let openChannelTask = async {
                do! lnd.ConnectTo serverWallet.NodeEndPoint
                return!
                    lnd.OpenChannel
                        serverWallet.NodeEndPoint
                        (Money(0.002m, MoneyUnit.BTC))
                        feeRate
            }

            let! acceptChannelRes, openChannelRes = AsyncExtensions.MixedParallel2 acceptChannelTask openChannelTask
            let (channelId, _) = UnwrapResult acceptChannelRes "AcceptChannel failed"
            UnwrapResult openChannelRes "lnd.OpenChannel failed"

            // Wait for the funding transaction to appear in mempool
            while bitcoind.GetTxIdsInMempool().Length = 0 do
                Thread.Sleep 500

            // Mine blocks on top of the funding transaction to make it confirmed.
            let minimumDepth = BlockHeightOffset32 6u
            bitcoind.GenerateBlocksToDummyAddress minimumDepth

            do! serverWallet.WaitForFundingConfirmed channelId

            let! lockFundingRes = Lightning.Network.AcceptLockChannelFunding serverWallet.NodeServer channelId
            UnwrapResult lockFundingRes "LockChannelFunding failed"

            let channelInfo = serverWallet.ChannelStore.ChannelInfo channelId
            match channelInfo.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            return channelId, serverWallet, bitcoind, electrumServer, lnd
        }

    let AcceptChannelFromGeewalletFunder () =
        async {
            let! serverWallet = ServerWalletInstance.New Config.FundeeLightningIPEndpoint (Some Config.FundeeAccountsPrivateKey)
            let! pendingChannelRes =
                Lightning.Network.AcceptChannel
                    serverWallet.NodeServer

            let (channelId, _) = UnwrapResult pendingChannelRes "OpenChannel failed"

            let! lockFundingRes = Lightning.Network.AcceptLockChannelFunding serverWallet.NodeServer channelId
            UnwrapResult lockFundingRes "LockChannelFunding failed"

            let channelInfo = serverWallet.ChannelStore.ChannelInfo channelId
            match channelInfo.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            if Money(channelInfo.Balance, MoneyUnit.BTC) <> Money(0.0m, MoneyUnit.BTC) then
                failwith "incorrect balance after accepting channel"

            return serverWallet, channelId
        }

    let ClientCloseChannel (clientWallet: ClientWalletInstance) (bitcoind: Bitcoind) channelId =
        async {
            let! closeChannelRes = Lightning.Network.CloseChannel clientWallet.NodeClient channelId
            match closeChannelRes with
            | Ok _ -> ()
            | Error err -> failwith (SPrintF1 "error when closing channel: %s" (err :> IErrorMsg).Message)

            match (clientWallet.ChannelStore.ChannelInfo channelId).Status with
            | ChannelStatus.Closing -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Closing, got %A" status)

            // Mine 10 blocks to make sure closing tx is confirmed
            bitcoind.GenerateBlocksToDummyAddress (BlockHeightOffset32 (uint32 10))

            let rec waitForClosingTxConfirmed attempt = async {
                Infrastructure.LogDebug (SPrintF1 "Checking if closing tx is finished, attempt #%d" attempt)
                if attempt = 10 then
                    return Error "Closing tx not confirmed after maximum attempts"
                else
                    let! txIsConfirmed = Lightning.Network.CheckClosingFinished (clientWallet.ChannelStore.ChannelInfo channelId)
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

    let SendHtlcPaymentsToLnd (clientWallet: ClientWalletInstance)
                              (lnd: Lnd)
                              (channelId: ChannelIdentifier)
                              (fundingAmount: Money) =
        async {
            let channelInfoBeforeAnyPayment = clientWallet.ChannelStore.ChannelInfo channelId
            match channelInfoBeforeAnyPayment.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            let! sendHtlcPayment1Res =
                async {
                    let transferAmount =
                        let accountBalance = Money(channelInfoBeforeAnyPayment.SpendableBalance, MoneyUnit.BTC)
                        TransferAmount (walletToWalletTestPayment1Amount.ToDecimal MoneyUnit.BTC, accountBalance.ToDecimal MoneyUnit.BTC, Currency.BTC)
                    let! invoiceOpt = 
                        lnd.CreateInvoice(transferAmount)
                    let invoice = UnwrapOption invoiceOpt "Failed to create first invoice"
                    let paymentRequest =
                        UnwrapResult (PaymentRequest.Parse invoice.BOLT11) "failed to parse payment request 1"

                    return! 
                        Lightning.Network.SendHtlcPayment
                            clientWallet.NodeClient
                            channelId
                            transferAmount
                            paymentRequest.PaymentHash.Value
                            paymentRequest.PaymentSecret
                            (NBitcoin.DataEncoders.HexEncoder().DecodeData(invoice.Id))
                }
            UnwrapResult sendHtlcPayment1Res "SendHtlcPayment failed"

            let channelInfoAfterPayment1 = clientWallet.ChannelStore.ChannelInfo channelId
            match channelInfoAfterPayment1.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            let! lndBalanceAfterPayment1 = lnd.ChannelBalance()

            if Money(channelInfoAfterPayment1.Balance, MoneyUnit.BTC) <> fundingAmount - walletToWalletTestPayment1Amount then
                failwith "incorrect balance after payment 1"
            if lndBalanceAfterPayment1 <> walletToWalletTestPayment1Amount then
                failwith "incorrect lnd balance after payment 1"

            let! sendHtlcPayment2Res =
                async {
                    let transferAmount =
                        let accountBalance = Money(channelInfoBeforeAnyPayment.SpendableBalance, MoneyUnit.BTC)
                        TransferAmount (walletToWalletTestPayment2Amount.ToDecimal MoneyUnit.BTC, accountBalance.ToDecimal MoneyUnit.BTC, Currency.BTC)
                    let! invoiceOpt = 
                        lnd.CreateInvoice(transferAmount)
                    let invoice = UnwrapOption invoiceOpt "Failed to create second invoice"
                    let paymentRequest =
                        UnwrapResult (PaymentRequest.Parse invoice.BOLT11) "failed to parse payment request 2"

                    return! 
                        Lightning.Network.SendHtlcPayment
                            clientWallet.NodeClient
                            channelId
                            transferAmount
                            paymentRequest.PaymentHash.Value
                            paymentRequest.PaymentSecret
                            (NBitcoin.DataEncoders.HexEncoder().DecodeData(invoice.Id))
                }
            UnwrapResult sendHtlcPayment2Res "SendHtlcPayment failed"

            let channelInfoAfterPayment2 = clientWallet.ChannelStore.ChannelInfo channelId
            match channelInfoAfterPayment2.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)
            let! lndBalanceAfterPayment2 = lnd.ChannelBalance()

            if Money(channelInfoAfterPayment2.Balance, MoneyUnit.BTC) <> fundingAmount - walletToWalletTestPayment1Amount - walletToWalletTestPayment2Amount then
                failwith "incorrect balance after payment 2"
            if lndBalanceAfterPayment2 <> lndBalanceAfterPayment1 + walletToWalletTestPayment2Amount then
                failwith "incorrect lnd balance after payment 2"
        }

    let SendMonoHopPayments (clientWallet: ClientWalletInstance) channelId fundingAmount =
        async {
            let channelInfoBeforeAnyPayment = clientWallet.ChannelStore.ChannelInfo channelId
            match channelInfoBeforeAnyPayment.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            let! sendMonoHopPayment1Res =
                let transferAmount =
                    let accountBalance = Money(channelInfoBeforeAnyPayment.SpendableBalance, MoneyUnit.BTC)
                    TransferAmount (walletToWalletTestPayment1Amount.ToDecimal MoneyUnit.BTC, accountBalance.ToDecimal MoneyUnit.BTC, Currency.BTC)
                Lightning.Network.SendMonoHopPayment
                    clientWallet.NodeClient
                    channelId
                    transferAmount
            UnwrapResult sendMonoHopPayment1Res "SendMonoHopPayment failed"

            let channelInfoAfterPayment1 = clientWallet.ChannelStore.ChannelInfo channelId
            match channelInfoAfterPayment1.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            if Money(channelInfoAfterPayment1.Balance, MoneyUnit.BTC) <> fundingAmount - walletToWalletTestPayment1Amount then
                failwith "incorrect balance after payment 1"

            let! sendMonoHopPayment2Res =
                let transferAmount =
                    let accountBalance = Money(channelInfoAfterPayment1.SpendableBalance, MoneyUnit.BTC)
                    TransferAmount (
                        walletToWalletTestPayment2Amount.ToDecimal MoneyUnit.BTC,
                        accountBalance.ToDecimal MoneyUnit.BTC, Currency.BTC
                    )
                Lightning.Network.SendMonoHopPayment
                    clientWallet.NodeClient
                    channelId
                    transferAmount
            UnwrapResult sendMonoHopPayment2Res "SendMonoHopPayment failed"

            let channelInfoAfterPayment2 = clientWallet.ChannelStore.ChannelInfo channelId
            match channelInfoAfterPayment2.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            if Money(channelInfoAfterPayment2.Balance, MoneyUnit.BTC) <> fundingAmount - walletToWalletTestPayment1Amount - walletToWalletTestPayment2Amount then
                failwith "incorrect balance after payment 2"
        }

    let ReceiveMonoHopPayments (serverWallet: ServerWalletInstance) channelId =
        async {
            let channelInfoBeforeAnyPayment = serverWallet.ChannelStore.ChannelInfo channelId
            match channelInfoBeforeAnyPayment.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            let balanceBeforeAnyPayment = Money(channelInfoBeforeAnyPayment.Balance, MoneyUnit.BTC)

            let! receiveMonoHopPaymentRes =
                Lightning.Network.ReceiveMonoHopPayment serverWallet.NodeServer channelId
            UnwrapResult receiveMonoHopPaymentRes "ReceiveMonoHopPayment failed"

            let channelInfoAfterPayment1 = serverWallet.ChannelStore.ChannelInfo channelId
            match channelInfoAfterPayment1.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            if Money(channelInfoAfterPayment1.Balance, MoneyUnit.BTC) <> balanceBeforeAnyPayment + walletToWalletTestPayment1Amount then
                failwith "incorrect balance after receiving payment 1"

            let! receiveMonoHopPaymentRes =
                Lightning.Network.ReceiveMonoHopPayment serverWallet.NodeServer channelId
            UnwrapResult receiveMonoHopPaymentRes "ReceiveMonoHopPayment failed"

            let channelInfoAfterPayment2 = serverWallet.ChannelStore.ChannelInfo channelId
            match channelInfoAfterPayment2.Status with
            | ChannelStatus.Active -> ()
            | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

            if Money(channelInfoAfterPayment2.Balance, MoneyUnit.BTC) <> balanceBeforeAnyPayment + walletToWalletTestPayment1Amount + walletToWalletTestPayment2Amount then
                failwith "incorrect balance after receiving payment 2"
        }


    [<Category "G2G_ChannelOpening_Funder">]
    [<Test>]
    member __.``can open channel with geewallet (funder)``() = Async.RunSynchronously <| async {
        let! _channelId, clientWallet, bitcoind, electrumServer, lnd, _fundingAmount =
            OpenChannelWithFundee (Some Config.FundeeNodeEndpoint)

        TearDown clientWallet lnd electrumServer bitcoind
    }

    [<Category "G2G_ChannelOpening_Fundee">]
    [<Test>]
    member __.``can open channel with geewallet (fundee)``() = Async.RunSynchronously <| async {
        let! serverWallet, _channelId = AcceptChannelFromGeewalletFunder ()

        (serverWallet :> IDisposable).Dispose()
    }

    [<Category "G2G_ChannelClosingAfterJustOpening_Funder">]
    [<Test>]
    member __.``can close channel with geewallet (funder)``() = Async.RunSynchronously <| async {
        let! channelId, clientWallet, bitcoind, electrumServer, lnd, _fundingAmount =
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

        do! ClientCloseChannel clientWallet bitcoind channelId

        TearDown clientWallet lnd electrumServer bitcoind
    }

    [<Category "G2G_ChannelClosingAfterJustOpening_Fundee">]
    [<Test>]
    member __.``can close channel with geewallet (fundee)``() = Async.RunSynchronously <| async {
        let! serverWallet, channelId = AcceptChannelFromGeewalletFunder ()

        let! closeChannelRes = Lightning.Network.AcceptCloseChannel serverWallet.NodeServer channelId
        match closeChannelRes with
        | Ok _ -> ()
        | Error err -> failwith (SPrintF1 "failed to accept close channel: %A" err)
    }

    [<Category "G2G_MonoHopUnidirectionalPayments_Funder">]
    [<Test>]
    member __.``can send monohop payments (funder)``() = Async.RunSynchronously <| async {
        let! channelId, clientWallet, bitcoind, electrumServer, lnd, fundingAmount =
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

        do! SendMonoHopPayments clientWallet channelId fundingAmount

        TearDown clientWallet lnd electrumServer bitcoind
    }


    [<Category "G2G_MonoHopUnidirectionalPayments_Fundee">]
    [<Test>]
    member __.``can receive mono-hop unidirectional payments, with geewallet (fundee)``() = Async.RunSynchronously <| async {
        let! serverWallet, channelId = AcceptChannelFromGeewalletFunder ()

        do! ReceiveMonoHopPayments serverWallet channelId

        (serverWallet :> IDisposable).Dispose()
    }

    [<Category "G2G_ChannelClosingAfterSendingMonoHopPayments_Funder">]
    [<Test>]
    member __.``can close channel after sending monohop payments (funder)``() = Async.RunSynchronously <| async {
        let! channelId, clientWallet, bitcoind, electrumServer, lnd, fundingAmount =
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
            do! SendMonoHopPayments clientWallet channelId fundingAmount
        with
        | ex ->
            Assert.Inconclusive (
                sprintf
                    "Channel-closing inconclusive because sending of monohop payments failed, fix this first: %s"
                    (ex.ToString())
            )
            failwith "unreachable"

        do! ClientCloseChannel clientWallet bitcoind channelId

        TearDown clientWallet lnd electrumServer bitcoind
    }


    [<Category "G2G_ChannelClosingAfterSendingMonoHopPayments_Fundee">]
    [<Test>]
    member __.``can close channel after receiving mono-hop unidirectional payments, with geewallet (fundee)``() = Async.RunSynchronously <| async {
        let! serverWallet, channelId = AcceptChannelFromGeewalletFunder ()

        do! ReceiveMonoHopPayments serverWallet channelId

        let! closeChannelRes = Lightning.Network.AcceptCloseChannel serverWallet.NodeServer channelId
        match closeChannelRes with
        | Ok _ -> ()
        | Error err -> failwith (SPrintF1 "failed to accept close channel: %A" err)

        (serverWallet :> IDisposable).Dispose()
    }

    [<Test>]
    member __.``can open channel with LND``() = Async.RunSynchronously <| async {
        let! _channelId, clientWallet, bitcoind, electrumServer, lnd, _fundingAmount = OpenChannelWithFundee None

        TearDown clientWallet lnd electrumServer bitcoind
    }

    [<Test>]
    member __.``can open channel with LND and send htlcs``() = Async.RunSynchronously <| async {
        let! channelId, clientWallet, bitcoind, electrumServer, lnd, fundingAmount = OpenChannelWithFundee None

        do! SendHtlcPaymentsToLnd clientWallet lnd channelId fundingAmount

        TearDown clientWallet lnd electrumServer bitcoind
    }


    [<Test>]
    member __.``can close channel with LND``() = Async.RunSynchronously <| async {
        let! channelId, clientWallet, bitcoind, electrumServer, lnd, _fundingAmount =
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

        let! closeChannelRes = Lightning.Network.CloseChannel clientWallet.NodeClient channelId
        match closeChannelRes with
        | Ok _ -> ()
        | Error err -> failwith (SPrintF1 "error when closing channel: %s" (err :> IErrorMsg).Message)

        match (clientWallet.ChannelStore.ChannelInfo channelId).Status with
        | ChannelStatus.Closing -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Closing, got %A" status)

        // Mine 10 blocks to make sure closing tx is confirmed
        bitcoind.GenerateBlocksToDummyAddress (BlockHeightOffset32 (uint32 10))

        let rec waitForClosingTxConfirmed attempt = async {
            Infrastructure.LogDebug (SPrintF1 "Checking if closing tx is finished, attempt #%d" attempt)
            if attempt = 10 then
                return Error "Closing tx not confirmed after maximum attempts"
            else
                let! txIsConfirmed = Lightning.Network.CheckClosingFinished (clientWallet.ChannelStore.ChannelInfo channelId)
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

        TearDown clientWallet lnd electrumServer bitcoind
    }

    [<Test>]
    member __.``can accept channel from LND``() = Async.RunSynchronously <| async {
        let! _channelId, serverWallet, bitcoind, electrumServer, lnd = AcceptChannelFromLndFunder ()

        TearDown serverWallet lnd electrumServer bitcoind
    }

    [<Test>]
    member __.``can accept channel closure from LND``() = Async.RunSynchronously <| async {
        let! channelId, serverWallet, bitcoind, electrumServer, lnd =
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

        let channelInfo = serverWallet.ChannelStore.ChannelInfo channelId

        // wait for lnd to realise we're offline
        do! Async.Sleep 1000
        let fundingOutPoint =
            let fundingTxId = uint256(channelInfo.FundingTxId.ToString())
            let fundingOutPointIndex = channelInfo.FundingOutPointIndex
            OutPoint(fundingTxId, fundingOutPointIndex)
        let closeChannelTask = async {
            do! lnd.ConnectTo serverWallet.NodeEndPoint
            do! Async.Sleep 1000
            do! lnd.CloseChannel fundingOutPoint
            return ()
        }
        let awaitCloseTask = async {
            let rec receiveEvent () = async {
                let! receivedEvent = Lightning.Network.ReceiveLightningEvent serverWallet.NodeServer channelId
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
            bitcoind.GenerateBlocksToDummyAddress minimumDepth
            return ()
        }

        let! (), () = AsyncExtensions.MixedParallel2 closeChannelTask awaitCloseTask

        TearDown serverWallet lnd electrumServer bitcoind

        return ()
    }

    [<Category "G2G_ChannelLocalForceClosing_Funder">]
    [<Test>]
    member __.``can send monohop payments and handle local force-close of channel (funder)``() = Async.RunSynchronously <| async {
        let! channelId, clientWallet, bitcoind, electrumServer, lnd, fundingAmount =
            try
                OpenChannelWithFundee (Some Config.FundeeNodeEndpoint)
            with
            | ex ->
                Assert.Inconclusive (
                    sprintf
                        "Channel local-force-closing inconclusive because Channel open failed, fix this first: %s"
                        (ex.ToString())
                )
                failwith "unreachable"

        try
            do! SendMonoHopPayments clientWallet channelId fundingAmount
        with
        | ex ->
            Assert.Inconclusive (
                sprintf
                    "Channel local-force-closing inconclusive because sending of monohop payments failed, fix this first: %s"
                    (ex.ToString())
            )
            failwith "unreachable"

        let! _forceCloseTxId = (Lightning.Node.Client clientWallet.NodeClient).ForceCloseChannel channelId

        let locallyForceClosedData =
            match (clientWallet.ChannelStore.ChannelInfo channelId).Status with
            | ChannelStatus.LocallyForceClosed locallyForceClosedData ->
                locallyForceClosedData
            | status -> failwith (SPrintF1 "unexpected channel status. Expected LocallyForceClosed, got %A" status)

        // wait for force-close transaction to appear in mempool
        while bitcoind.GetTxIdsInMempool().Length = 0 do
            do! Async.Sleep 500

        Infrastructure.LogDebug (SPrintF1 "the time lock is %i blocks" locallyForceClosedData.ToSelfDelay)

        let! balanceBeforeFundsReclaimed = clientWallet.GetBalance()

        // Mine the force-close tx into a block
        bitcoind.GenerateBlocksToDummyAddress (BlockHeightOffset32 1u)

        // Mine blocks to release time-lock
        bitcoind.GenerateBlocksToDummyAddress
            (BlockHeightOffset32 (uint32 locallyForceClosedData.ToSelfDelay))

        let! _spendingTxId =
            UtxoCoin.Account.BroadcastRawTransaction
                locallyForceClosedData.Currency
                locallyForceClosedData.SpendingTransactionString

        // wait for spending transaction to appear in mempool
        while bitcoind.GetTxIdsInMempool().Length = 0 do
            do! Async.Sleep 500

        // Mine the spending tx into a block
        bitcoind.GenerateBlocksToDummyAddress (BlockHeightOffset32 1u)

        Infrastructure.LogDebug "waiting for our wallet balance to increase"
        let! _balanceAfterFundsReclaimed =
            let amount = balanceBeforeFundsReclaimed + Money(1.0m, MoneyUnit.Satoshi)
            clientWallet.WaitForBalance amount

        TearDown clientWallet lnd electrumServer bitcoind
    }

    [<Category "G2G_ChannelLocalForceClosing_Fundee">]
    [<Test>]
    member __.``can receive monohop payments and handle local force-close of channel (fundee)``() = Async.RunSynchronously <| async {
        let! serverWallet, channelId =
            try
                AcceptChannelFromGeewalletFunder ()
            with
            | ex ->
                Assert.Inconclusive (
                    sprintf
                        "Channel local-force-closing inconclusive because Channel accept failed, fix this first: %s"
                        (ex.ToString())
                )
                failwith "unreachable"

        try
            do! ReceiveMonoHopPayments serverWallet channelId
        with
        | ex ->
            Assert.Inconclusive (
                sprintf
                    "Channel remote-force-closing inconclusive because receiving of monohop payments failed, fix this first: %s"
                    (ex.ToString())
            )
            failwith "unreachable"

        (serverWallet :> IDisposable).Dispose()
    }

    [<Category "G2G_ChannelRemoteForceClosingByFunder_Funder">]
    [<Test>]
    member __.``can send monohop payments and handle remote force-close of channel by funder (funder)``() = Async.RunSynchronously <| async {
        let! channelId, clientWallet, bitcoind, electrumServer, lnd, fundingAmount =
            try
                OpenChannelWithFundee (Some Config.FundeeNodeEndpoint)
            with
            | ex ->
                Assert.Inconclusive (
                    sprintf
                        "Channel remote-force-closing inconclusive because Channel open failed, fix this first: %s"
                        (ex.ToString())
                )
                failwith "unreachable"

        try
            do! SendMonoHopPayments clientWallet channelId fundingAmount
        with
        | ex ->
            Assert.Inconclusive (
                sprintf
                    "Channel remote-force-closing inconclusive because sending of monohop payments failed, fix this first: %s"
                    (ex.ToString())
            )
            failwith "unreachable"

        let! _forceCloseTxId = (Lightning.Node.Client clientWallet.NodeClient).ForceCloseChannel channelId

        let locallyForceClosedData =
            match (clientWallet.ChannelStore.ChannelInfo channelId).Status with
            | ChannelStatus.LocallyForceClosed locallyForceClosedData ->
                locallyForceClosedData
            | status -> failwith (SPrintF1 "unexpected channel status. Expected LocallyForceClosed, got %A" status)

        // wait for force-close transaction to appear in mempool
        while bitcoind.GetTxIdsInMempool().Length = 0 do
            do! Async.Sleep 500

        Infrastructure.LogDebug (SPrintF1 "the time lock is %i blocks" locallyForceClosedData.ToSelfDelay)

        let! balanceBeforeFundsReclaimed = clientWallet.GetBalance()

        // Mine the force-close tx into a block
        bitcoind.GenerateBlocksToDummyAddress (BlockHeightOffset32 1u)

        // wait for fundee's recovery tx to appear in mempool
        while bitcoind.GetTxIdsInMempool().Length = 0 do
            do! Async.Sleep 500

        // Mine the fundee's recovery tx
        bitcoind.GenerateBlocksToDummyAddress (BlockHeightOffset32 1u)

        // Mine blocks to release time-lock
        bitcoind.GenerateBlocksToDummyAddress
            (BlockHeightOffset32 (uint32 locallyForceClosedData.ToSelfDelay))

        let! _spendingTxId =
            UtxoCoin.Account.BroadcastRawTransaction
                locallyForceClosedData.Currency
                locallyForceClosedData.SpendingTransactionString

        // wait for spending transaction to appear in mempool
        while bitcoind.GetTxIdsInMempool().Length = 0 do
            do! Async.Sleep 500

        // Mine the spending tx into a block
        bitcoind.GenerateBlocksToDummyAddress (BlockHeightOffset32 1u)

        Infrastructure.LogDebug "waiting for our wallet balance to increase"
        let! _balanceAfterFundsReclaimed =
            let amount = balanceBeforeFundsReclaimed + Money(1.0m, MoneyUnit.Satoshi)
            clientWallet.WaitForBalance amount

        // Give the fundee time to see their funds recovered before closing bitcoind/electrum
        do! Async.Sleep 3000

        TearDown clientWallet lnd electrumServer bitcoind
    }

    [<Category "G2G_ChannelRemoteForceClosingByFunder_Fundee">]
    [<Test>]
    member __.``can receive monohop payments and handle remote force-close of channel by funder (fundee)``() = Async.RunSynchronously <| async {
        let! serverWallet, channelId =
            try
                AcceptChannelFromGeewalletFunder ()
            with
            | ex ->
                Assert.Inconclusive (
                    sprintf
                        "Channel remote-force-closing inconclusive because Channel accept failed, fix this first: %s"
                        (ex.ToString())
                )
                failwith "unreachable"

        try
            do! ReceiveMonoHopPayments serverWallet channelId
        with
        | ex ->
            Assert.Inconclusive (
                sprintf
                    "Channel remote-force-closing inconclusive because receiving of monohop payments failed, fix this first: %s"
                    (ex.ToString())
            )
            failwith "unreachable"

        let! balanceBeforeFundsReclaimed = serverWallet.GetBalance()

        let rec waitForRemoteForceClose() = async {
            let! closingInfoOpt = serverWallet.ChannelStore.CheckForClosingTx channelId
            match closingInfoOpt with
            | Some (ClosingTx.ForceClose closingTx, Some _closingTxHeight) ->
                return!
                    (Node.Server serverWallet.NodeServer).CreateRecoveryTxForRemoteForceClose
                        channelId
                        closingTx
                        false
            | _ ->
                do! Async.Sleep 2000
                return! waitForRemoteForceClose()
        }
        let! recoveryTxStringOpt = waitForRemoteForceClose()
        let recoveryTxString = UnwrapResult recoveryTxStringOpt "no funds could be recovered"
        let! _recoveryTxId =
            UtxoCoin.Account.BroadcastRawTransaction
                Currency.BTC
                recoveryTxString

        Infrastructure.LogDebug ("waiting for our wallet balance to increase")
        let! _balanceAfterFundsReclaimed =
            let amount = balanceBeforeFundsReclaimed+ Money(1.0m, MoneyUnit.Satoshi)
            serverWallet.WaitForBalance amount

        (serverWallet :> IDisposable).Dispose()
    }

    [<Category "G2G_ChannelRemoteForceClosingByFundee_Funder">]
    [<Test>]
    member __.``can send monohop payments and handle remote force-close of channel by fundee (funder)``() = Async.RunSynchronously <| async {
        let! channelId, clientWallet, bitcoind, electrumServer, lnd, fundingAmount =
            try
                OpenChannelWithFundee (Some Config.FundeeNodeEndpoint)
            with
            | ex ->
                Assert.Inconclusive (
                    sprintf
                        "Channel remote-force-closing inconclusive because Channel open failed, fix this first: %s"
                        (ex.ToString())
                )
                failwith "unreachable"

        try
            do! SendMonoHopPayments clientWallet channelId fundingAmount
        with
        | ex ->
            Assert.Inconclusive (
                sprintf
                    "Channel remote-force-closing inconclusive because sending of monohop payments failed, fix this first: %s"
                    (ex.ToString())
            )
            failwith "unreachable"

        // wait for force-close transaction to appear in mempool
        while bitcoind.GetTxIdsInMempool().Length = 0 do
            do! Async.Sleep 500

        let! balanceBeforeFundsReclaimed = clientWallet.GetBalance()

        // Mine the force-close tx into a block
        bitcoind.GenerateBlocksToDummyAddress (BlockHeightOffset32 1u)

        let! closingInfoOpt = clientWallet.ChannelStore.CheckForClosingTx channelId
        let closingTx, _closingTxHeightOpt = UnwrapOption closingInfoOpt "force close tx not found on blockchain"

        let forceCloseTx =
            match closingTx with
            | ClosingTx.ForceClose forceCloseTx ->
                forceCloseTx
            | _ -> failwith "closing tx is not a force close tx"

        let! recoveryTxStringOpt =
            (Node.Client clientWallet.NodeClient).CreateRecoveryTxForRemoteForceClose
                channelId
                forceCloseTx
                false
        let recoveryTxString = UnwrapResult recoveryTxStringOpt "no funds could be recovered"
        let! _recoveryTxId =
            UtxoCoin.Account.BroadcastRawTransaction
                Currency.BTC
                recoveryTxString

        // wait for our recovery tx to appear in mempool
        while bitcoind.GetTxIdsInMempool().Length = 0 do
            do! Async.Sleep 500

        // Mine our recovery tx
        bitcoind.GenerateBlocksToDummyAddress (BlockHeightOffset32 1u)

        Infrastructure.LogDebug ("waiting for our wallet balance to increase")
        let! _balanceAfterFundsReclaimed =
            let amount = balanceBeforeFundsReclaimed+ Money(1.0m, MoneyUnit.Satoshi)
            clientWallet.WaitForBalance amount

        // mine blocks while waiting for the fundee's recovery tx to appear in mempool
        while bitcoind.GetTxIdsInMempool().Length = 0 do
            bitcoind.GenerateBlocksToDummyAddress (BlockHeightOffset32 1u)
            do! Async.Sleep 500

        // Mine the fundee's recovery tx
        bitcoind.GenerateBlocksToDummyAddress (BlockHeightOffset32 1u)

        // Give the fundee time to see their funds recovered before closing bitcoind/electrum
        do! Async.Sleep 10000

        TearDown clientWallet lnd electrumServer bitcoind
    }

    [<Category "G2G_ChannelRemoteForceClosingByFundee_Fundee">]
    [<Test>]
    member __.``can receive monohop payments and handle remote force-close of channel by fundee (fundee)``() = Async.RunSynchronously <| async {
        let! serverWallet, channelId =
            try
                AcceptChannelFromGeewalletFunder ()
            with
            | ex ->
                Assert.Inconclusive (
                    sprintf
                        "Channel remote-force-closing inconclusive because Channel accept failed, fix this first: %s"
                        (ex.ToString())
                )
                failwith "unreachable"

        try
            do! ReceiveMonoHopPayments serverWallet channelId
        with
        | ex ->
            Assert.Inconclusive (
                sprintf
                    "Channel remote-force-closing inconclusive because receiving of monohop payments failed, fix this first: %s"
                    (ex.ToString())
            )
            failwith "unreachable"

        let! _forceCloseTxId = (Node.Server serverWallet.NodeServer).ForceCloseChannel channelId

        let locallyForceClosedData =
            match (serverWallet.ChannelStore.ChannelInfo channelId).Status with
            | ChannelStatus.LocallyForceClosed locallyForceClosedData ->
                locallyForceClosedData
            | status -> failwith (SPrintF1 "unexpected channel status. Expected LocallyForceClosed, got %A" status)

        Infrastructure.LogDebug (SPrintF1 "the time lock is %i blocks" locallyForceClosedData.ToSelfDelay)

        let! balanceBeforeFundsReclaimed = serverWallet.GetBalance()

        let rec waitForTimeLockExpired() = async {
            let! remainingConfirmations = locallyForceClosedData.GetRemainingConfirmations()
            if remainingConfirmations > 0us then
                do! Async.Sleep 500
                return! waitForTimeLockExpired()
        }
        do! waitForTimeLockExpired()

        let! _spendingTxId =
            UtxoCoin.Account.BroadcastRawTransaction
                locallyForceClosedData.Currency
                locallyForceClosedData.SpendingTransactionString

        Infrastructure.LogDebug "waiting for our wallet balance to increase"
        let! _balanceAfterFundsReclaimed =
            let amount = balanceBeforeFundsReclaimed + Money(1.0m, MoneyUnit.Satoshi)
            serverWallet.WaitForBalance amount

        (serverWallet :> IDisposable).Dispose()
    }

    [<Category "G2G_Revocation_Funder">]
    [<Test>]
    member __.``can revoke commitment tx (funder)``() = Async.RunSynchronously <| async {
        use! walletInstance = ClientWalletInstance.New None
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
        bitcoind.GenerateBlocks maturityDurationInNumberOfBlocks lndAddress

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
        let! feeRate = ElectrumServer.EstimateFeeRate()
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
        bitcoind.GenerateBlocks consideredConfirmedAmountOfBlocksPlusOne lndAddress

        let fundingAmount = Money(0.1m, MoneyUnit.BTC)
        let! transferAmount = async {
            let! accountBalance = walletInstance.WaitForBalance fundingAmount
            return TransferAmount (fundingAmount.ToDecimal MoneyUnit.BTC, accountBalance.ToDecimal MoneyUnit.BTC, Currency.BTC)
        }
        let! metadata = ChannelManager.EstimateChannelOpeningFee (walletInstance.Account :?> NormalUtxoAccount) transferAmount
        let! pendingChannelRes =
            Lightning.Network.OpenChannel
                walletInstance.NodeClient
                Config.FundeeNodeEndpoint
                transferAmount
        let pendingChannel = UnwrapResult pendingChannelRes "OpenChannel failed"
        let minimumDepth = (pendingChannel :> IChannelToBeOpened).ConfirmationsRequired
        let! acceptRes = pendingChannel.Accept metadata walletInstance.Password
        let (channelId, _fundingTxId) = UnwrapResult acceptRes "pendingChannel.Accept failed"
        bitcoind.GenerateBlocks (BlockHeightOffset32 minimumDepth) lndAddress

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

        let! lockFundingRes = Lightning.Network.ConnectLockChannelFunding walletInstance.NodeClient channelId
        UnwrapResult lockFundingRes "LockChannelFunding failed"

        let channelInfo = walletInstance.ChannelStore.ChannelInfo channelId
        match channelInfo.Status with
        | ChannelStatus.Active -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

        if Money(channelInfo.Balance, MoneyUnit.BTC) <> fundingAmount then
            failwith "balance does not match funding amount"

        let! sendMonoHopPayment1Res =
            let transferAmount =
                let accountBalance = Money(channelInfo.SpendableBalance, MoneyUnit.BTC)
                TransferAmount (walletToWalletTestPayment1Amount.ToDecimal MoneyUnit.BTC, accountBalance.ToDecimal MoneyUnit.BTC, Currency.BTC)
            Lightning.Network.SendMonoHopPayment
                walletInstance.NodeClient
                channelId
                transferAmount
        UnwrapResult sendMonoHopPayment1Res "SendMonoHopPayment failed"

        let channelInfoAfterPayment1 = walletInstance.ChannelStore.ChannelInfo channelId
        match channelInfo.Status with
        | ChannelStatus.Active -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

        if Money(channelInfoAfterPayment1.Balance, MoneyUnit.BTC) <> fundingAmount - walletToWalletTestPayment1Amount then
            failwith "incorrect balance after payment 1"

        let commitmentTx = walletInstance.ChannelStore.GetCommitmentTx channelId

        let! sendMonoHopPayment2Res =
            let transferAmount =
                let accountBalance = Money(channelInfo.SpendableBalance, MoneyUnit.BTC)
                TransferAmount (walletToWalletTestPayment2Amount.ToDecimal MoneyUnit.BTC, accountBalance.ToDecimal MoneyUnit.BTC, Currency.BTC)
            Lightning.Network.SendMonoHopPayment
                walletInstance.NodeClient
                channelId
                transferAmount
        UnwrapResult sendMonoHopPayment2Res "SendMonoHopPayment failed"

        let channelInfoAfterPayment2 = walletInstance.ChannelStore.ChannelInfo channelId
        match channelInfo.Status with
        | ChannelStatus.Active -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

        if Money(channelInfoAfterPayment2.Balance, MoneyUnit.BTC) <> fundingAmount - walletToWalletTestPayment1Amount - walletToWalletTestPayment2Amount then
            failwith "incorrect balance after payment 1"

        let! _theftTxId = UtxoCoin.Account.BroadcastRawTransaction Currency.BTC commitmentTx

        // wait for theft transaction to appear in mempool
        while bitcoind.GetTxIdsInMempool().Length = 0 do
            do! Async.Sleep 500

        // mine the theft tx into a block
        bitcoind.GenerateBlocks (BlockHeightOffset32 1u) lndAddress

        let! accountBalanceBeforeSpendingTheftTx =
            walletInstance.GetBalance()

        // give the fundee plenty of time to broadcast the penalty tx
        do! Async.Sleep 3000

        // mine enough blocks to allow broadcasting the spending tx
        let toSelfDelay = walletInstance.ChannelStore.GetToSelfDelay channelId
        bitcoind.GenerateBlocks (BlockHeightOffset32 (uint32 toSelfDelay)) lndAddress

        let! spendingTxStringRes =
            (Node.Client walletInstance.NodeClient).CreateRecoveryTxForLocalForceClose
                channelId
                commitmentTx
                false
        let spendingTxString = UnwrapResult spendingTxStringRes "failed to create spending tx"
        let! spendingTxIdOpt = async {
            try
                let! spendingTxId = UtxoCoin.Account.BroadcastRawTransaction Currency.BTC spendingTxString
                return Some spendingTxId
            with
            | ex ->
                let _exns = FindSingleException<UtxoCoin.ElectrumServerReturningErrorException> ex
                return None
        }

        match spendingTxIdOpt with
        | Some spendingTxId ->
            failwith (SPrintF1 "successfully broadcast spending tx (%s)" spendingTxId)
        | None -> ()

        let! accountBalanceAfterSpendingTheftTx =
            walletInstance.GetBalance()

        if accountBalanceBeforeSpendingTheftTx <> accountBalanceAfterSpendingTheftTx then
            failwithf
                "Unexpected account balance! before theft tx == %A, after theft tx == %A"
                accountBalanceBeforeSpendingTheftTx
                accountBalanceAfterSpendingTheftTx

        // give the fundee plenty of time to see that their tx was mined
        do! Async.Sleep 5000

        return ()
    }

    [<Category "G2G_Revocation_Fundee">]
    [<Test>]
    member __.``can revoke commitment tx (fundee)``() = Async.RunSynchronously <| async {
        use! walletInstance = ServerWalletInstance.New Config.FundeeLightningIPEndpoint (Some Config.FundeeAccountsPrivateKey)
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

        let channelInfoAfterPayment1 = walletInstance.ChannelStore.ChannelInfo channelId
        match channelInfo.Status with
        | ChannelStatus.Active -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

        if Money(channelInfoAfterPayment1.Balance, MoneyUnit.BTC) <> walletToWalletTestPayment1Amount then
            failwith "incorrect balance after receiving payment 1"

        let! receiveMonoHopPaymentRes =
            Lightning.Network.ReceiveMonoHopPayment walletInstance.NodeServer channelId
        UnwrapResult receiveMonoHopPaymentRes "ReceiveMonoHopPayment failed"

        let channelInfoAfterPayment2 = walletInstance.ChannelStore.ChannelInfo channelId
        match channelInfo.Status with
        | ChannelStatus.Active -> ()
        | status -> failwith (SPrintF1 "unexpected channel status. Expected Active, got %A" status)

        if Money(channelInfoAfterPayment2.Balance, MoneyUnit.BTC) <> walletToWalletTestPayment1Amount + walletToWalletTestPayment2Amount then
            failwith "incorrect balance after receiving payment 1"

        // attempt to broadcast tx which spends the theft tx
        let rec checkForClosingTx() = async {
            let! txIdOpt =
                (Node.Server walletInstance.NodeServer).CheckForChannelFraudAndSendRevocationTx
                    channelId
            match txIdOpt with
            | None ->
                do! Async.Sleep 500
                return! checkForClosingTx()
            | Some _ ->
                return ()
        }
        do! checkForClosingTx()
        let! _accountBalance =
            // wait for any amount of money to appear in the wallet
            let amount = Money(1.0m, MoneyUnit.Satoshi)
            walletInstance.WaitForBalance amount

        return ()
    }

    [<Category "G2G_CPFP_Funder">]
    [<Test>]
    member __.``CPFP is used when force-closing channel (funder)``() = Async.RunSynchronously <| async {
        let! channelId, clientWallet, bitcoind, electrumServer, lnd, fundingAmount =
            try
                OpenChannelWithFundee (Some Config.FundeeNodeEndpoint)
            with
            | ex ->
                Assert.Inconclusive (
                    sprintf
                        "CPFP inconclusive because Channel open failed, fix this first: %s"
                        (ex.ToString())
                )
                failwith "unreachable"

        try
            do! SendMonoHopPayments clientWallet channelId fundingAmount
        with
        | ex ->
            Assert.Inconclusive (
                sprintf
                    "CPFP inconclusive because sending of monohop payments failed, fix this first: %s"
                    (ex.ToString())
            )
            failwith "unreachable"

        let! oldFeeRate = ElectrumServer.EstimateFeeRate()

        let newFeeRate = oldFeeRate * 5u
        ElectrumServer.SetEstimatedFeeRate newFeeRate

        let commitmentTxString = clientWallet.ChannelStore.GetCommitmentTx channelId
        let! _commitmentTxIdString = Account.BroadcastRawTransaction Currency.BTC commitmentTxString

        let commitmentTx = Transaction.Parse(commitmentTxString, Network.RegTest)
        let! commitmentTxFee = FeesHelper.GetFeeFromTransaction commitmentTx
        let commitmentTxFeeRate =
            FeeRatePerKw.FromFeeAndVSize(commitmentTxFee, uint64 (commitmentTx.GetVirtualSize()))
        assert FeesHelper.FeeRatesApproxEqual commitmentTxFeeRate oldFeeRate

        let! recoveryTxStringNoCpfpOpt =
            (Node.Client clientWallet.NodeClient).CreateRecoveryTxForLocalForceClose
                channelId
                commitmentTxString
                false
        let recoveryTxStringNoCpfp =
            UnwrapResult
                recoveryTxStringNoCpfpOpt
                "force close failed to recover funds from the commitment tx"
        let recoveryTxNoCpfp = Transaction.Parse(recoveryTxStringNoCpfp, Network.RegTest)
        let! recoveryTxFeeNoCpfp = FeesHelper.GetFeeFromTransaction recoveryTxNoCpfp
        let recoveryTxFeeRateNoCpfp =
            FeeRatePerKw.FromFeeAndVSize(recoveryTxFeeNoCpfp, uint64 (recoveryTxNoCpfp.GetVirtualSize()))
        assert FeesHelper.FeeRatesApproxEqual recoveryTxFeeRateNoCpfp newFeeRate
        let combinedFeeRateNoCpfp =
            FeeRatePerKw.FromFeeAndVSize(
                recoveryTxFeeNoCpfp + commitmentTxFee,
                uint64 (recoveryTxNoCpfp.GetVirtualSize() + commitmentTx.GetVirtualSize())
            )
        assert (not <| FeesHelper.FeeRatesApproxEqual combinedFeeRateNoCpfp oldFeeRate)
        assert (not <| FeesHelper.FeeRatesApproxEqual combinedFeeRateNoCpfp newFeeRate)

        let! recoveryTxStringWithCpfpOpt =
            (Node.Client clientWallet.NodeClient).CreateRecoveryTxForLocalForceClose
                channelId
                commitmentTxString
                true
        let recoveryTxStringWithCpfp =
            UnwrapResult
                recoveryTxStringWithCpfpOpt
                "force close failed to recover funds from the commitment tx"
        let recoveryTxWithCpfp = Transaction.Parse(recoveryTxStringWithCpfp, Network.RegTest)
        let! recoveryTxFeeWithCpfp = FeesHelper.GetFeeFromTransaction recoveryTxWithCpfp
        let recoveryTxFeeRateWithCpfp =
            FeeRatePerKw.FromFeeAndVSize(recoveryTxFeeWithCpfp, uint64 (recoveryTxWithCpfp.GetVirtualSize()))
        assert (not <| FeesHelper.FeeRatesApproxEqual recoveryTxFeeRateWithCpfp oldFeeRate)
        assert (not <| FeesHelper.FeeRatesApproxEqual recoveryTxFeeRateWithCpfp newFeeRate)
        let combinedFeeRateWithCpfp =
            FeeRatePerKw.FromFeeAndVSize(
                recoveryTxFeeWithCpfp + commitmentTxFee,
                uint64 (recoveryTxWithCpfp.GetVirtualSize() + commitmentTx.GetVirtualSize())
            )
        assert FeesHelper.FeeRatesApproxEqual combinedFeeRateWithCpfp newFeeRate

        // Give the fundee time to see the force-close tx
        do! Async.Sleep 5000

        TearDown clientWallet lnd electrumServer bitcoind
    }

    [<Category "G2G_CPFP_Fundee">]
    [<Test>]
    member __.``CPFP is used when force-closing channel (fundee)``() = Async.RunSynchronously <| async {
        let! serverWallet, channelId =
            try
                AcceptChannelFromGeewalletFunder ()
            with
            | ex ->
                Assert.Inconclusive (
                    sprintf
                        "Channel remote-force-closing inconclusive because Channel accept failed, fix this first: %s"
                        (ex.ToString())
                )
                failwith "unreachable"

        try
            do! ReceiveMonoHopPayments serverWallet channelId
        with
        | ex ->
            Assert.Inconclusive (
                sprintf
                    "CPFP inconclusive because receiving of monohop payments failed, fix this first: %s"
                    (ex.ToString())
            )
            failwith "unreachable"

        let! oldFeeRate = ElectrumServer.EstimateFeeRate()
        let newFeeRate = oldFeeRate * 5u
        ElectrumServer.SetEstimatedFeeRate newFeeRate

        let rec waitForForceClose(): Async<ForceCloseTx> = async {
            let! closingTxInfoOpt = serverWallet.ChannelStore.CheckForClosingTx channelId
            match closingTxInfoOpt with
            | Some (ClosingTx.ForceClose forceCloseTx, _blockHeightOpt) ->
                return forceCloseTx
            | Some (ClosingTx.MutualClose _mutualCloseTx, _blockHeightOpt) ->
                return failwith "should not happen"
            | None ->
                do! Async.Sleep 500
                return! waitForForceClose()
        }

        let! wrappedForceCloseTx = waitForForceClose()
        let forceCloseTxString = wrappedForceCloseTx.Tx.ToString()
        let forceCloseTx = Transaction.Parse (forceCloseTxString, Network.RegTest)
        let! forceCloseTxFee = FeesHelper.GetFeeFromTransaction forceCloseTx
        let forceCloseTxFeeRate =
            FeeRatePerKw.FromFeeAndVSize(forceCloseTxFee, uint64 (forceCloseTx.GetVirtualSize()))
        assert FeesHelper.FeeRatesApproxEqual forceCloseTxFeeRate oldFeeRate

        let! recoveryTxStringNoCpfpRes =
            (Node.Server serverWallet.NodeServer).CreateRecoveryTxForRemoteForceClose
                channelId
                wrappedForceCloseTx
                false
        let recoveryTxStringNoCpfp =
            UnwrapResult
                recoveryTxStringNoCpfpRes
                "force close failed to recover funds from the commitment tx"
        let recoveryTxNoCpfp = Transaction.Parse(recoveryTxStringNoCpfp, Network.RegTest)
        let! recoveryTxFeeNoCpfp = FeesHelper.GetFeeFromTransaction recoveryTxNoCpfp
        let recoveryTxFeeRateNoCpfp =
            FeeRatePerKw.FromFeeAndVSize(recoveryTxFeeNoCpfp, uint64 (recoveryTxNoCpfp.GetVirtualSize()))
        assert FeesHelper.FeeRatesApproxEqual recoveryTxFeeRateNoCpfp newFeeRate
        let combinedFeeRateNoCpfp =
            FeeRatePerKw.FromFeeAndVSize(
                recoveryTxFeeNoCpfp + forceCloseTxFee,
                uint64 (recoveryTxNoCpfp.GetVirtualSize() + forceCloseTx.GetVirtualSize())
            )
        assert (not <| FeesHelper.FeeRatesApproxEqual combinedFeeRateNoCpfp oldFeeRate)
        assert (not <| FeesHelper.FeeRatesApproxEqual combinedFeeRateNoCpfp newFeeRate)

        let! recoveryTxStringWithCpfpRes =
            (Node.Server serverWallet.NodeServer).CreateRecoveryTxForRemoteForceClose
                channelId
                wrappedForceCloseTx
                true
        let recoveryTxStringWithCpfp =
            UnwrapResult
                recoveryTxStringWithCpfpRes
                "force close failed to recover funds from the commitment tx"
        let recoveryTxWithCpfp = Transaction.Parse(recoveryTxStringWithCpfp, Network.RegTest)
        let! recoveryTxFeeWithCpfp = FeesHelper.GetFeeFromTransaction recoveryTxWithCpfp
        let recoveryTxFeeRateWithCpfp =
            FeeRatePerKw.FromFeeAndVSize(recoveryTxFeeWithCpfp, uint64 (recoveryTxWithCpfp.GetVirtualSize()))
        assert (not <| FeesHelper.FeeRatesApproxEqual recoveryTxFeeRateWithCpfp oldFeeRate)
        assert (not <| FeesHelper.FeeRatesApproxEqual recoveryTxFeeRateWithCpfp newFeeRate)
        let combinedFeeRateWithCpfp =
            FeeRatePerKw.FromFeeAndVSize(
                recoveryTxFeeWithCpfp + forceCloseTxFee,
                uint64 (recoveryTxWithCpfp.GetVirtualSize() + forceCloseTx.GetVirtualSize())
            )
        assert FeesHelper.FeeRatesApproxEqual combinedFeeRateWithCpfp newFeeRate

        (serverWallet :> IDisposable).Dispose()
    }

    [<Category "G2G_UpdateFeeMsg_Funder">]
    [<Test>]
    member __.``can update fee after sending monohop payments (funder)``() = Async.RunSynchronously <| async {
        let! channelId, clientWallet, bitcoind, electrumServer, lnd, fundingAmount =
            try
                OpenChannelWithFundee (Some Config.FundeeNodeEndpoint)
            with
            | ex ->
                Assert.Inconclusive (
                    sprintf
                        "UpdateFee message support inconclusive because Channel open failed, fix this first: %s"
                        (ex.ToString())
                )
                failwith "unreachable"
        let! feeRate = ElectrumServer.EstimateFeeRate()

        try
            do! SendMonoHopPayments clientWallet channelId fundingAmount
        with
        | ex ->
            Assert.Inconclusive (
                sprintf
                    "UpdateFee message support inconclusive because sending of monohop payments failed, fix this first: %s"
                    (ex.ToString())
            )
            failwith "unreachable"

        ElectrumServer.SetEstimatedFeeRate (feeRate * 4u)
        let! newFeeRateOpt = clientWallet.ChannelStore.FeeUpdateRequired channelId
        let newFeeRate = UnwrapOption newFeeRateOpt "Fee update should be required"
        let! updateFeeRes =
            (Node.Client clientWallet.NodeClient).UpdateFee channelId newFeeRate
        UnwrapResult updateFeeRes "UpdateFee failed"

        let channelInfoAfterUpdateMessageFee = clientWallet.ChannelStore.ChannelInfo channelId
        let currentBalance = Money(channelInfoAfterUpdateMessageFee.Balance, MoneyUnit.BTC)
        try
            do! SendMonoHopPayments clientWallet channelId currentBalance
        with
        | ex ->
            Assert.Inconclusive (
                sprintf
                    "Sending of monohop payments failed after UpdateFee message handling: %s"
                    (ex.ToString())
            )
            failwith "unreachable"

        do! ClientCloseChannel clientWallet bitcoind channelId

        TearDown clientWallet lnd electrumServer bitcoind
    }

    [<Category "G2G_UpdateFeeMsg_Fundee">]
    [<Test>]
    member __.``can accept fee update after receiving mono-hop unidirectional payments, with geewallet (fundee)``() = Async.RunSynchronously <| async {
        let! serverWallet, channelId =
            try
                AcceptChannelFromGeewalletFunder ()
            with
            | ex ->
                Assert.Inconclusive (
                    sprintf
                        "UpdateFee message support inconclusive because Channel accept failed, fix this first: %s"
                        (ex.ToString())
                )
                failwith "unreachable"

        let! feeRate = ElectrumServer.EstimateFeeRate()

        try
            do! ReceiveMonoHopPayments serverWallet channelId
        with
        | ex ->
            Assert.Inconclusive (
                sprintf
                    "UpdateFee message support inconclusive because receiving of monohop payments failed, fix this first: %s"
                    (ex.ToString())
            )
            failwith "unreachable"

        ElectrumServer.SetEstimatedFeeRate (feeRate * 4u)
        let! acceptUpdateFeeRes =
            Lightning.Network.AcceptUpdateFee serverWallet.NodeServer channelId
        UnwrapResult acceptUpdateFeeRes "AcceptUpdateFee failed"

        try
            do! ReceiveMonoHopPayments serverWallet channelId
        with
        | ex ->
            Assert.Inconclusive (
                sprintf
                    "Receiving of monohop payments failed after UpdateFee message handling: %s"
                    (ex.ToString())
            )
            failwith "unreachable"

        let! closeChannelRes = Lightning.Network.AcceptCloseChannel serverWallet.NodeServer channelId
        match closeChannelRes with
        | Ok _ -> ()
        | Error err -> failwith (SPrintF1 "failed to accept close channel: %A" err)

        (serverWallet :> IDisposable).Dispose()
    }

