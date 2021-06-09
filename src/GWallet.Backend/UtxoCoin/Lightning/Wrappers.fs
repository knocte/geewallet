namespace GWallet.Backend.UtxoCoin.Lightning

open System

open NBitcoin
open DotNetLightning.Channel
open DotNetLightning.Chain
open DotNetLightning.Utils
open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.FSharpUtil.UwpHacks


type ChannelIdentifier =
    internal {
        DnlChannelId: DotNetLightning.Utils.Primitives.ChannelId
    }
    static member internal FromDnl (dnlChannelId: DotNetLightning.Utils.Primitives.ChannelId): ChannelIdentifier =
        { DnlChannelId = dnlChannelId }

    static member NewRandom(): ChannelIdentifier =
        let dnlChannelId =
            let random = Org.BouncyCastle.Security.SecureRandom() :> Random
            let temporaryChannelIdBytes: array<byte> = Array.zeroCreate 32
            random.NextBytes temporaryChannelIdBytes
            temporaryChannelIdBytes
            |> NBitcoin.uint256
            |> DotNetLightning.Utils.Primitives.ChannelId
        { DnlChannelId = dnlChannelId }

    static member Parse (text: string): Option<ChannelIdentifier> =
        try
            let dnlChannelId =
                text
                |> NBitcoin.uint256
                |> DotNetLightning.Utils.Primitives.ChannelId
            Some { DnlChannelId = dnlChannelId }
        with
        | :? FormatException -> None

    override self.ToString() =
        self.DnlChannelId.Value.ToString()


type TransactionIdentifier =
    internal {
        DnlTxId: DotNetLightning.Utils.Primitives.TxId
    }
    static member internal FromHash (txIdHash: NBitcoin.uint256): TransactionIdentifier =
        { DnlTxId = DotNetLightning.Utils.Primitives.TxId txIdHash }

    override self.ToString() =
        self.DnlTxId.Value.ToString()

type MonoHopUnidirectionalChannel =
    internal {
        Channel: Channel
    }
    static member internal Create (nodeId: NodeId)
                                  (account: UtxoCoin.NormalUtxoAccount)
                                  (nodeSecret: ExtKey)
                                  (channelIndex: int)
                                  (fundingTxProvider: ProvideFundingTx)
                                  (initialState: ChannelState)
                                      : Async<MonoHopUnidirectionalChannel> = async {
        let shutdownScriptPubKey = ScriptManager.CreatePayoutScript account
        let channelConfig: DotNetLightning.Utils.ChannelConfig =
            let handshakeConfig: DotNetLightning.Utils.ChannelHandshakeConfig = {
                MinimumDepth = BlockHeightOffset32 1u
            }
            let channelOptions: DotNetLightning.Utils.ChannelOptions = {
                AnnounceChannel = false
                FeeProportionalMillionths = 100u
                MaxFeeRateMismatchRatio = 1.
                ShutdownScriptPubKey = Some shutdownScriptPubKey
            }
            {
                ChannelHandshakeConfig = handshakeConfig
                PeerChannelConfigLimits = Settings.PeerLimits
                ChannelOptions = channelOptions
            }
        let keyRepo = DefaultKeyRepository(nodeSecret, channelIndex)
        let currency = (account :> IAccount).Currency
        let! feeEstimator = FeeEstimator.Create currency
        let network = UtxoCoin.Account.GetNetwork currency
        let channel =
            Channel.Create(
                channelConfig,
                keyRepo,
                feeEstimator,
                nodeSecret.PrivateKey,
                fundingTxProvider,
                network,
                nodeId
            )
        let channel = { channel with State = initialState }
        return { Channel = channel }
    }

    member internal self.RemoteNodeId
        with get(): NodeId = self.Channel.RemoteNodeId

    member internal self.Network
        with get(): Network = self.Channel.Network

    member self.ChannelId
        with get(): Option<ChannelIdentifier> =
            match self.Channel.State.ChannelId with
            | Some channelId ->
                Some <| ChannelIdentifier.FromDnl channelId
            | None -> None

    member internal self.ChannelKeys
        with get(): ChannelKeys =
            self.Channel.KeysRepository.GetChannelKeys false

    member self.FundingTxId
        with get(): Option<TransactionIdentifier> =
            match self.Channel.State.Commitments with
            | Some commitments ->
                { DnlTxId = DotNetLightning.Utils.Primitives.TxId commitments.FundingScriptCoin.Outpoint.Hash }
                |> Some
            | None -> None

    member internal self.FundingScriptCoin
        with get(): Option<ScriptCoin> =
            match self.Channel.State.Commitments with
            | Some commitments -> Some commitments.FundingScriptCoin
            | None -> None

    member internal self.LocalParams (funding: Money)
                                     (defaultFinalScriptPubKey: Script)
                                     (isFunder: bool)
                                         : LocalParams =
        Settings.GetLocalParams funding
                                defaultFinalScriptPubKey
                                isFunder
                                self.RemoteNodeId
                                self.ChannelKeys

    member internal self.ExecuteCommand<'T> (channelCmd: ChannelCommand)
                                            (eventFilter: List<ChannelEvent> -> Option<'T>)
                                                : Result<'T, ChannelError> * MonoHopUnidirectionalChannel =
        match Channel.executeCommand self.Channel channelCmd with
        | Error channelError -> (Error channelError), self
        | Ok evtList ->
            match (eventFilter evtList) with
            | Some value ->
                let rec apply (channel: Channel) (evtList: List<ChannelEvent>) =
                    match evtList with
                    | evt::rest ->
                        let channel = Channel.applyEvent channel evt
                        apply channel rest
                    | [] -> channel
                let channelAfterEventsApplied = apply self.Channel evtList
                let channel = { self with Channel = channelAfterEventsApplied }
                (Ok value), channel
            | None ->
                failwith <| SPrintF2
                    "unexpected result executing channel command %A. got: %A"
                    channelCmd
                    evtList

    member internal self.Balance(): Option<LNMoney> =
        match self.Channel.State.Commitments with
        | Some commitments -> Some commitments.LocalCommit.Spec.ToLocal
        | None -> None

    member internal self.SpendableBalance(): Option<LNMoney> =
        match self.Channel.State.Commitments with
        | Some commitments -> Some <| commitments.SpendableBalance()
        | None -> None
