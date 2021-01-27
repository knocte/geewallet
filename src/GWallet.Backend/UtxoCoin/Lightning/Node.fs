namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.IO
open System.Net

open NBitcoin
open DotNetLightning.Channel
open DotNetLightning.Crypto
open DotNetLightning.Utils
open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks

type internal NodeOpenChannelError =
    | Connect of ConnectError
    | OpenChannel of OpenChannelError
    interface IErrorMsg with
        member self.Message =
            match self with
            | Connect connectError ->
                SPrintF1 "error connecting: %s" (connectError :> IErrorMsg).Message
            | OpenChannel openChannelError ->
                SPrintF1 "error opening channel: %s" (openChannelError :> IErrorMsg).Message

        member self.ChannelBreakdown: bool =
            false

type internal NodeAcceptChannelError =
    | AcceptPeer of ConnectError
    | AcceptChannel of AcceptChannelError
    interface IErrorMsg with
        member self.Message =
            match self with
            | AcceptPeer connectError ->
                SPrintF1 "error accepting connection: %s" (connectError :> IErrorMsg).Message
            | AcceptChannel acceptChannelError ->
                SPrintF1 "error accepting channel: %s" (acceptChannelError :> IErrorMsg).Message

        member self.ChannelBreakdown: bool =
            false

type internal NodeSendMonoHopPaymentError =
    | Reconnect of ReconnectActiveChannelError
    | SendPayment of SendMonoHopPaymentError
    interface IErrorMsg with
        member self.Message =
            match self with
            | Reconnect reconnectActiveChannelError ->
                SPrintF1 "error reconnecting channel: %s" (reconnectActiveChannelError :> IErrorMsg).Message
            | SendPayment sendMonoHopPaymentError ->
                SPrintF1 "error sending payment on reconnected channel: %s"
                         (sendMonoHopPaymentError :> IErrorMsg).Message

        member self.ChannelBreakdown: bool =
            match self with
            | Reconnect reconnectActiveChannelError ->
                (reconnectActiveChannelError :> IErrorMsg).ChannelBreakdown
            | SendPayment sendMonoHopPaymentError ->
                (sendMonoHopPaymentError :> IErrorMsg).ChannelBreakdown

type internal NodeReceiveMonoHopPaymentError =
    | Reconnect of ReconnectActiveChannelError
    | ReceivePayment of RecvMonoHopPaymentError
    interface IErrorMsg with
        member self.Message =
            match self with
            | Reconnect reconnectActiveChannelError ->
                SPrintF1 "error reconnecting channel: %s" (reconnectActiveChannelError :> IErrorMsg).Message
            | ReceivePayment recvMonoHopPaymentError ->
                SPrintF1 "error receiving payment on reconnected channel: %s"
                         (recvMonoHopPaymentError :> IErrorMsg).Message

        member self.ChannelBreakdown: bool =
            match self with
            | Reconnect reconnectActiveChannelError ->
                (reconnectActiveChannelError :> IErrorMsg).ChannelBreakdown
            | ReceivePayment recvMonoHopPaymentError ->
                (recvMonoHopPaymentError :> IErrorMsg).ChannelBreakdown

type internal NodeAcceptCloseChannelError =
    | Reconnect of ReconnectActiveChannelError
    | AcceptCloseChannel of CloseChannelError
    interface IErrorMsg with
        member self.Message =
            match self with
            | Reconnect reconnectActiveChannelError ->
                SPrintF1 "error reconnecting channel: %s" (reconnectActiveChannelError :> IErrorMsg).Message
            | AcceptCloseChannel acceptCloseChannelError ->
                SPrintF1 "error accepting channel close on reconnected channel: %s"
                         (acceptCloseChannelError :> IErrorMsg).Message

        member self.ChannelBreakdown: bool =
            match self with
            | Reconnect reconnectActiveChannelError ->
                (reconnectActiveChannelError :> IErrorMsg).ChannelBreakdown
            | AcceptCloseChannel acceptCloseChannelError ->
                (acceptCloseChannelError :> IErrorMsg).ChannelBreakdown

type internal NodeReceiveLightningEventError =
    | Reconnect of ReconnectActiveChannelError
    interface IErrorMsg with
        member self.Message =
            match self with
            | Reconnect reconnectActiveChannelError ->
                SPrintF1 "error reconnecting channel: %s" (reconnectActiveChannelError :> IErrorMsg).Message

        member self.ChannelBreakdown: bool =
            match self with
            | Reconnect reconnectActiveChannelError ->
                (reconnectActiveChannelError :> IErrorMsg).ChannelBreakdown

type IChannelToBeOpened =
    abstract member ConfirmationsRequired: uint32 with get
    abstract member ChannelId: ChannelIdentifier with get

type IncomingChannelEvent =
    | MonoHopUnidirectionalPayment
    | Shutdown

type PendingChannel internal (outgoingUnfundedChannel: OutgoingUnfundedChannel) =
    member internal self.OutgoingUnfundedChannel = outgoingUnfundedChannel
    member public self.Accept (): Async<Result<TransactionIdentifier, IErrorMsg>> = async {
        let! fundedChannelRes =
            FundedChannel.FundChannel self.OutgoingUnfundedChannel
        match fundedChannelRes with
        | Error fundChannelError ->
            if fundChannelError.PossibleBug then
                let msg = SPrintF1 "Error funding channel: %s" (fundChannelError :> IErrorMsg).Message
                Infrastructure.ReportWarningMessage msg
            return Error (fundChannelError :> IErrorMsg)
        | Ok fundedChannel ->
            let txId = fundedChannel.FundingTxId
            (fundedChannel :> IDisposable).Dispose()
            return Ok txId
    }
    interface IChannelToBeOpened with
        member self.ConfirmationsRequired
            with get(): uint32 =
                self.OutgoingUnfundedChannel.MinimumDepth.Value
        member self.ChannelId
            with get(): ChannelIdentifier =
                self.OutgoingUnfundedChannel.ChannelId

type Node internal (channelStore: ChannelStore, transportListener: TransportListener) =
    member val ChannelStore = channelStore
    member val internal TransportListener = transportListener
    member val internal NodeId = transportListener.NodeId
    member val EndPoint = transportListener.EndPoint
    member val Account = channelStore.Account

    interface IDisposable with
        member self.Dispose() =
            (self.TransportListener :> IDisposable).Dispose()

    static member internal AccountPrivateKeyToNodeMasterPrivKey (accountKey: Key): NodeMasterPrivKey =
        let privateKeyBytesLength = 32
        let bytes: array<byte> = Array.zeroCreate privateKeyBytesLength
        use bytesStream = new MemoryStream(bytes)
        let stream = NBitcoin.BitcoinStream(bytesStream, true)
        accountKey.ReadWrite stream
        NodeMasterPrivKey <| NBitcoin.ExtKey bytes

    static member AccountPrivateKeyToNodePubKey (accountKey: Key): PubKey =
        let nodeMasterPrivKey = Node.AccountPrivateKeyToNodeMasterPrivKey accountKey
        nodeMasterPrivKey.NodeId().Value

    member internal self.OpenChannel (nodeEndPoint: NodeEndPoint)
                                     (channelCapacity: TransferAmount)
                                     (metadata: TransactionMetadata)
                                     (password: string)
                                         : Async<Result<PendingChannel, IErrorMsg>> = async {
        let peerId = PeerId (nodeEndPoint.IPEndPoint :> EndPoint)
        let nodeId = nodeEndPoint.NodeId.ToString() |> NBitcoin.PubKey |> NodeId
        let! connectRes =
            PeerNode.ConnectFromTransportListener self.TransportListener nodeId peerId
        match connectRes with
        | Error connectError ->
            if connectError.PossibleBug then
                let msg =
                    SPrintF3
                        "error connecting to peer %s to open channel of capacity %M: %s"
                        (nodeEndPoint.ToString())
                        channelCapacity.ValueToSend
                        (connectError :> IErrorMsg).Message
                Infrastructure.ReportWarningMessage msg
            return Error <| (NodeOpenChannelError.Connect connectError :> IErrorMsg)
        | Ok peerNode ->
            let! outgoingUnfundedChannelRes =
                OutgoingUnfundedChannel.OpenChannel
                    peerNode
                    self.Account
                    channelCapacity
                    metadata
                    password
            match outgoingUnfundedChannelRes with
            | Error openChannelError ->
                if openChannelError.PossibleBug then
                    let msg =
                        SPrintF3
                            "error opening channel with peer %s of capacity %M: %s"
                            (nodeEndPoint.ToString())
                            channelCapacity.ValueToSend
                            (openChannelError :> IErrorMsg).Message
                    Infrastructure.ReportWarningMessage msg
                return Error <| (NodeOpenChannelError.OpenChannel openChannelError :> IErrorMsg)
            | Ok outgoingUnfundedChannel ->
                return Ok <| PendingChannel(outgoingUnfundedChannel)
    }

    member internal self.AcceptChannel (): Async<Result<(ChannelIdentifier * TransactionIdentifier), IErrorMsg>> = async {
        let! acceptPeerRes =
            PeerNode.AcceptAnyFromTransportListener self.TransportListener
        match acceptPeerRes with
        | Error connectError ->
            if connectError.PossibleBug then
                let msg =
                    SPrintF1
                        "error accepting connection from peer to accept channel: %s"
                        (connectError :> IErrorMsg).Message
                Infrastructure.ReportWarningMessage msg
            return Error <| (NodeAcceptChannelError.AcceptPeer connectError :> IErrorMsg)
        | Ok peerNode ->
            let! fundedChannelRes = FundedChannel.AcceptChannel peerNode self.Account
            match fundedChannelRes with
            | Error acceptChannelError ->
                if acceptChannelError.PossibleBug then
                    let msg =
                        SPrintF2
                            "error accepting channel from peer %s: %s"
                            (peerNode.NodeEndPoint.ToString())
                            (acceptChannelError :> IErrorMsg).Message
                    Infrastructure.ReportWarningMessage msg
                return Error <| (NodeAcceptChannelError.AcceptChannel acceptChannelError :> IErrorMsg)
            | Ok fundedChannel ->
                let channelId = fundedChannel.ChannelId
                let txId = fundedChannel.FundingTxId
                (fundedChannel :> IDisposable).Dispose()
                return Ok (channelId, txId)
    }

    member internal self.SendMonoHopPayment (channelId: ChannelIdentifier)
                                            (transferAmount: TransferAmount)
                                                : Async<Result<unit, IErrorMsg>> = async {
        let amount =
            let btcAmount = transferAmount.ValueToSend
            let lnAmount = int64(btcAmount * decimal DotNetLightning.Utils.LNMoneyUnit.BTC)
            DotNetLightning.Utils.LNMoney lnAmount
        let! activeChannelRes =
            ActiveChannel.ConnectReestablish self.ChannelStore self.TransportListener channelId
        match activeChannelRes with
        | Error reconnectActiveChannelError ->
            if reconnectActiveChannelError.PossibleBug then
                let msg =
                    SPrintF2
                        "error connecting to peer to send monohop payment on channel %s: %s"
                        (channelId.ToString())
                        (reconnectActiveChannelError :> IErrorMsg).Message
                Infrastructure.ReportWarningMessage msg
            return Error <| (NodeSendMonoHopPaymentError.Reconnect reconnectActiveChannelError :> IErrorMsg)
        | Ok activeChannel ->
            let! paymentRes = activeChannel.SendMonoHopUnidirectionalPayment amount
            match paymentRes with
            | Error sendMonoHopPaymentError ->
                if sendMonoHopPaymentError.PossibleBug then
                    let msg =
                        SPrintF2
                            "error sending monohop payment on channel %s: %s"
                            (channelId.ToString())
                            (sendMonoHopPaymentError :> IErrorMsg).Message
                    Infrastructure.ReportWarningMessage msg
                return Error <| (NodeSendMonoHopPaymentError.SendPayment sendMonoHopPaymentError :> IErrorMsg)
            | Ok activeChannelAfterPayment ->
                (activeChannelAfterPayment :> IDisposable).Dispose()
                return Ok ()
    }

    member internal self.ReceiveMonoHopPayment (channelId: ChannelIdentifier)
                                                   : Async<Result<unit, IErrorMsg>> = async {
        let! activeChannelRes = ActiveChannel.AcceptReestablish self.ChannelStore self.TransportListener channelId
        match activeChannelRes with
        | Error reconnectActiveChannelError ->
            if reconnectActiveChannelError.PossibleBug then
                let msg =
                    SPrintF2
                        "error accepting connection from peer to receive monohop payment on channel %s: %s"
                        (channelId.ToString())
                        (reconnectActiveChannelError :> IErrorMsg).Message
                Infrastructure.ReportWarningMessage msg
            return Error <| (NodeReceiveMonoHopPaymentError.Reconnect reconnectActiveChannelError :> IErrorMsg)
        | Ok activeChannel ->
            let connectedChannel = activeChannel.ConnectedChannel

            Infrastructure.LogDebug "Waiting for lightning message"
            let! recvChannelMsgRes = connectedChannel.PeerNode.RecvChannelMsg()
            match recvChannelMsgRes with
            | Error err ->
                return failwith <| SPrintF1 "Received error while waiting for lightning message: %s" (err :> IErrorMsg).Message
            | Ok (peerNodeAfterMsgReceived, channelMsg) ->
                match channelMsg with
                | :? DotNetLightning.Serialization.Msgs.MonoHopUnidirectionalPaymentMsg as monoHopUnidirectionalPaymentMsg ->
                    let activeChannelAfterMsgReceived = { activeChannel with
                                                            ConnectedChannel = { activeChannel.ConnectedChannel with
                                                                                    PeerNode = peerNodeAfterMsgReceived }}

                    return! self.HandleMonoHopUnidirectionalPaymentMsg activeChannelAfterMsgReceived channelId monoHopUnidirectionalPaymentMsg
                | msg ->
                    return failwith <| SPrintF1 "Unexpected msg while waiting for monohop payment message: %As" msg
    }

    member private self.HandleMonoHopUnidirectionalPaymentMsg (activeChannel: ActiveChannel)
                                                              (channelId: ChannelIdentifier)
                                                              (monoHopUnidirectionalPaymentMsg: DotNetLightning.Serialization.Msgs.MonoHopUnidirectionalPaymentMsg)
                                                                  : Async<Result<unit, IErrorMsg>> = async {
        let! paymentRes = activeChannel.RecvMonoHopUnidirectionalPayment monoHopUnidirectionalPaymentMsg
        match paymentRes with
        | Error recvMonoHopPaymentError ->
            if recvMonoHopPaymentError.PossibleBug then
                let msg =
                    SPrintF2
                        "error accepting monohop payment on channel %s: %s"
                        (channelId.ToString())
                        (recvMonoHopPaymentError :> IErrorMsg).Message
                Infrastructure.ReportWarningMessage msg
            return Error <| (NodeReceiveMonoHopPaymentError.ReceivePayment recvMonoHopPaymentError :> IErrorMsg)
        | Ok activeChannelAfterPaymentReceived ->
            (activeChannelAfterPaymentReceived :> IDisposable).Dispose()
            return Ok ()
    }

    member internal self.InitiateCloseChannel (channelId: ChannelIdentifier): Async<Result<unit, IErrorMsg>> =
        async {
            let! connectRes = ActiveChannel.ConnectReestablish self.ChannelStore self.TransportListener channelId
            match connectRes with
            | Error connectError ->
                return failwith <| SPrintF1 "Error reestablishing channel: %s" (connectError :> IErrorMsg).Message
            | Ok activeChannel ->
                let! closeRes = ClosedChannel.InitiateCloseChannel activeChannel.ConnectedChannel
                match closeRes with
                | Error closeError ->
                    return failwith <| SPrintF1 "Error closing channel: %s" (closeError :> IErrorMsg).Message
                | Ok _ ->
                    return Ok ()
        }

    member internal self.AcceptCloseChannel (channelId: ChannelIdentifier)
                                                : Async<Result<unit, IErrorMsg>> = async {
        let! activeChannelRes = ActiveChannel.AcceptReestablish self.ChannelStore self.TransportListener channelId
        match activeChannelRes with
        | Error reconnectActiveChannelError ->
            if reconnectActiveChannelError.PossibleBug then
                let msg =
                    SPrintF2
                        "error accepting connection from peer to accept close channel on channel %s: %s"
                        (channelId.ToString())
                        (reconnectActiveChannelError :> IErrorMsg).Message
                Infrastructure.ReportWarningMessage msg
            return Error <| (NodeAcceptCloseChannelError.Reconnect reconnectActiveChannelError :> IErrorMsg)
        | Ok activeChannel ->
            let connectedChannel = activeChannel.ConnectedChannel

            Infrastructure.LogDebug "Waiting for lightning message"
            let! recvChannelMsgRes = connectedChannel.PeerNode.RecvChannelMsg()
            match recvChannelMsgRes with
            | Error err ->
                return failwith <| SPrintF1 "Received error while waiting for lightning message: %s" (err :> IErrorMsg).Message
            | Ok (peerNodeAfterMsgReceived, channelMsg) ->
                match channelMsg with
                | :? DotNetLightning.Serialization.Msgs.ShutdownMsg as shutdownMsg ->
                    let activeChannelAfterMsgReceived = { activeChannel with
                                                            ConnectedChannel = { activeChannel.ConnectedChannel with
                                                                                    PeerNode = peerNodeAfterMsgReceived }}

                    return! self.HandleShutdownMsg activeChannelAfterMsgReceived shutdownMsg
                | msg ->
                    return failwith <| SPrintF1 "Unexpected msg while waiting for shutdown message: %As" msg
    }

    member private self.HandleShutdownMsg (activeChannel: ActiveChannel)
                                          (shutdownMsg: DotNetLightning.Serialization.Msgs.ShutdownMsg)
                                              : Async<Result<unit, IErrorMsg>> = async {
        let! closeRes = ClosedChannel.AcceptCloseChannel (activeChannel.ConnectedChannel, shutdownMsg)
        match closeRes with
        | Error acceptCloseChannelError ->
            return Error <| (NodeAcceptCloseChannelError.AcceptCloseChannel acceptCloseChannelError :> IErrorMsg)
        | Ok _ ->
            return Ok ()
    }

    member internal self.LockChannelFunding (channelId: ChannelIdentifier)
                                                : Async<Result<unit, IErrorMsg>> =
        async {
            let channelInfo = self.ChannelStore.ChannelInfo channelId
            let! activeChannelRes =
                if channelInfo.IsFunder then
                    ActiveChannel.ConnectReestablish
                        self.ChannelStore
                        self.TransportListener
                        channelId
                else
                    ActiveChannel.AcceptReestablish
                        self.ChannelStore
                        self.TransportListener
                        channelId
            match activeChannelRes with
            | Error reconnectActiveChannelError ->
                if reconnectActiveChannelError.PossibleBug then
                    let msg =
                        SPrintF2
                            "error reconnecting to peer to lock channel %s: %s"
                            (channelId.ToString())
                            (reconnectActiveChannelError :> IErrorMsg).Message
                    Infrastructure.ReportWarningMessage msg
                return Error (reconnectActiveChannelError :> IErrorMsg)
            | Ok activeChannel ->
                (activeChannel :> IDisposable).Dispose()
                return Ok ()
        }

    member internal self.ReceiveLightningEvent (channelId: ChannelIdentifier)
                                            : Async<Result<IncomingChannelEvent, IErrorMsg>> = async {
        let rec receiveEvent (activeChannel: ActiveChannel) = async {
            Infrastructure.LogDebug "Waiting for lightning message"
            let connectedChannel = activeChannel.ConnectedChannel
            let peerNode = connectedChannel.PeerNode
            let! recvChannelMsgRes = peerNode.RecvChannelMsg()
            match recvChannelMsgRes with
            | Error err ->
                return failwith <| SPrintF1 "Received error while waiting for lightning message: %s" (err :> IErrorMsg).Message
            | Ok (peerNodeAfterMsgReceived, channelMsg) ->
                let connectedChannelAfterMsgReceived = {
                    connectedChannel with
                        PeerNode = peerNodeAfterMsgReceived
                }
                let activeChannelAfterMsgReceived = {
                    activeChannel with
                        ConnectedChannel = connectedChannelAfterMsgReceived
                }
                match channelMsg with
                | :? DotNetLightning.Serialization.Msgs.MonoHopUnidirectionalPaymentMsg as monoHopUnidirectionalPaymentMsg ->
                    let! res = self.HandleMonoHopUnidirectionalPaymentMsg activeChannelAfterMsgReceived channelId monoHopUnidirectionalPaymentMsg
                    match res with
                    | Error err -> return Error err
                    | Ok () -> return Ok IncomingChannelEvent.MonoHopUnidirectionalPayment
                | :? DotNetLightning.Serialization.Msgs.ShutdownMsg as shutdownMsg ->
                    let! res = self.HandleShutdownMsg activeChannelAfterMsgReceived shutdownMsg
                    match res with
                    | Error err -> return Error err
                    | Ok () -> return Ok IncomingChannelEvent.Shutdown
                | _ ->
                    Infrastructure.LogDebug <| SPrintF2 "Ignoring this msg (%A): %A" (channelMsg.GetType()) channelMsg
                    return! receiveEvent activeChannelAfterMsgReceived
        }
        let! activeChannelRes = ActiveChannel.AcceptReestablish self.ChannelStore self.TransportListener channelId
        match activeChannelRes with
        | Error reconnectActiveChannelError ->
            if reconnectActiveChannelError.PossibleBug then
                let msg =
                    SPrintF2
                        "error accepting connection from peer to receive lightning event on channel %s: %s"
                        (channelId.ToString())
                        (reconnectActiveChannelError :> IErrorMsg).Message
                Infrastructure.ReportWarningMessage msg
            return Error <| (NodeReceiveLightningEventError.Reconnect reconnectActiveChannelError :> IErrorMsg)
        | Ok activeChannel -> return! receiveEvent activeChannel
    }

    member self.ForceCloseUsingCommitmentTx (commitmentTxString: string)
                                            (channelId: ChannelIdentifier)
                                                : Async<Option<string>> = async {
        let account = self.Account :> IAccount
        let currency = account.Currency
        let serializedChannel = self.ChannelStore.LoadChannel channelId
        let! recoveryTransactionStringOpt = async {
            let network =
                UtxoCoin.Account.GetNetwork currency
            let commitmentTx =
                Transaction.Parse(commitmentTxString, network)
            let commitments = 
                ChannelSerialization.Commitments serializedChannel
            let channelPrivKeys =
                let channelIndex = serializedChannel.ChannelIndex
                let nodeMasterPrivKey = self.TransportListener.NodeMasterPrivKey
                nodeMasterPrivKey.ChannelPrivKeys channelIndex
            let targetAddress =
                let originAddress = account.PublicAddress
                BitcoinAddress.Create(originAddress, network)
            let! feeRate = async {
                let! feeEstimator = FeeEstimator.Create currency
                return feeEstimator.FeeRatePerKw
            }
            let recoveryTransactionOpt =
                ForceCloseFundsRecovery.tryGetFundsFromLocalCommitmentTx
                    commitments
                    channelPrivKeys
                    network
                    commitmentTx
                    targetAddress
                    feeRate
                    true
            match recoveryTransactionOpt with
            | Some recoveryTransaction -> return Some <| recoveryTransaction.ToHex()
            | None -> return None
        }
        return recoveryTransactionStringOpt
    }

    member self.ForceClose (channelId: ChannelIdentifier)
                               : Async<Option<string>> = async {
        let commitmentTxString = self.ChannelStore.GetCommitmentTx channelId
        let serializedChannel = self.ChannelStore.LoadChannel channelId
        let! forceCloseTxId =
            let currency =
                let account = self.Account :> IAccount
                account.Currency
            UtxoCoin.Account.BroadcastRawTransaction currency commitmentTxString
        let! recoveryTxStringOpt = self.ForceCloseUsingCommitmentTx commitmentTxString channelId
        match recoveryTxStringOpt with
        | None ->
            self.ChannelStore.DeleteChannel channelId
            return None
        | Some recoveryTxString ->
            let newSerializedChannel = {
                serializedChannel with
                    LocalForceCloseSpendingTxOpt = Some recoveryTxString
            }
            self.ChannelStore.SaveChannel newSerializedChannel
            return Some forceCloseTxId
    }

    member self.CreateRecoveryTxForRemoteForceClose (channelId: ChannelIdentifier)
                                                    (closingTxId: string)
                                                    (requiresCpfp: bool)
                                                        : Async<Option<string>> = async {
        let serializedChannel = self.ChannelStore.LoadChannel channelId
        let commitments = ChannelSerialization.Commitments serializedChannel
        let account = self.Account :> IAccount
        let currency = account.Currency
        let network = UtxoCoin.Account.GetNetwork currency
        Console.WriteLine(SPrintF1 "creating recovery tx for closing txid %s" closingTxId)
        let! closingTxHex =
            Server.Query
                currency
                (QuerySettings.Default ServerSelectionMode.Fast)
                (ElectrumClient.GetBlockchainTransaction (closingTxId.ToString()))
                None
        let closingTx = Transaction.Parse(closingTxHex, network)
        let channelPrivKeys =
            let channelIndex = serializedChannel.ChannelIndex
            let nodeMasterPrivKey = self.TransportListener.NodeMasterPrivKey
            nodeMasterPrivKey.ChannelPrivKeys channelIndex
        let targetAddress =
            let originAddress = account.PublicAddress
            BitcoinAddress.Create(originAddress, network)
        let! feeRate = async {
            let! feeEstimator = FeeEstimator.Create currency
            return feeEstimator.FeeRatePerKw
        }
        let recoveryTxOpt =
            ForceCloseFundsRecovery.tryGetFundsFromRemoteCommitmentTx
                commitments
                channelPrivKeys
                network
                closingTx
                targetAddress
                feeRate
                requiresCpfp
        match recoveryTxOpt with
        | Some recoveryTx -> return Some <| recoveryTx.ToHex()
        | None -> return None
    }

module public Connection =
    let public Start (channelStore: ChannelStore)
                     (password: string)
                     (bindAddress: IPEndPoint)
                         : Node =
        let privateKey = Account.GetPrivateKey channelStore.Account password
        let nodeMasterPrivKey: NodeMasterPrivKey = Node.AccountPrivateKeyToNodeMasterPrivKey privateKey
        let transportListener = TransportListener.Bind nodeMasterPrivKey bindAddress
        new Node (channelStore, transportListener)
