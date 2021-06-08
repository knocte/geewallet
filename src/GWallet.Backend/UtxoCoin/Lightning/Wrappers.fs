namespace GWallet.Backend.UtxoCoin.Lightning

open System

open NBitcoin
open DotNetLightning.Channel
open DotNetLightning.Chain
open DotNetLightning.Crypto
open DotNetLightning.Utils
open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.FSharpUtil.UwpHacks


type ChannelIdentifier =
    internal {
        DnlChannelId: DotNetLightning.Utils.ChannelId
    }
    static member internal FromDnl (dnlChannelId: DotNetLightning.Utils.ChannelId): ChannelIdentifier =
        { DnlChannelId = dnlChannelId }

    static member NewRandom(): ChannelIdentifier =
        let dnlChannelId =
            let random = Org.BouncyCastle.Security.SecureRandom() :> Random
            let temporaryChannelIdBytes: array<byte> = Array.zeroCreate 32
            random.NextBytes temporaryChannelIdBytes
            temporaryChannelIdBytes
            |> NBitcoin.uint256
            |> DotNetLightning.Utils.ChannelId
        { DnlChannelId = dnlChannelId }

    static member Parse (text: string): Option<ChannelIdentifier> =
        try
            let dnlChannelId =
                text
                |> NBitcoin.uint256
                |> DotNetLightning.Utils.ChannelId
            Some { DnlChannelId = dnlChannelId }
        with
        | :? FormatException -> None

    override self.ToString() =
        self.DnlChannelId.Value.ToString()


type TransactionIdentifier =
    internal {
        DnlTxId: DotNetLightning.Utils.TxId
    }
    static member internal FromHash (txIdHash: NBitcoin.uint256): TransactionIdentifier =
        { DnlTxId = DotNetLightning.Utils.TxId txIdHash }

    override self.ToString() =
        self.DnlTxId.Value.ToString()

type UtxoTransaction =
    internal {
        NbTx: NBitcoin.Transaction
    }

    override self.ToString() =
        self.NbTx.ToHex()

type MonoHopUnidirectionalChannel =
    internal {
        Channel: Channel
    }

    // 1 means actually 3x here, for more info see https://github.com/joemphilips/DotNetLightning/commit/0914d9e609d0a93bed50de1636e97590e9ff5aaa#diff-5545b0c089cff9618299dfafd5d9fa1d97b6762e4f977b78625f3c8c7266f5faR341
    static member internal DefaultMaxFeeRateMismatchRatio: float = 1.

    static member internal Create (nodeId: NodeId)
                                  (account: UtxoCoin.NormalUtxoAccount)
                                  (nodeMasterPrivKey: NodeMasterPrivKey)
                                  (channelIndex: int)
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
                MaxFeeRateMismatchRatio =
                    MonoHopUnidirectionalChannel.DefaultMaxFeeRateMismatchRatio
                ShutdownScriptPubKey = Some shutdownScriptPubKey
            }
            {
                ChannelHandshakeConfig = handshakeConfig
                PeerChannelConfigLimits = Settings.PeerLimits
                ChannelOptions = channelOptions
            }
        let currency = (account :> IAccount).Currency
        let! feeEstimator = FeeEstimator.Create currency
        let network = UtxoCoin.Account.GetNetwork currency
        let channel =
            Channel.Create(
                channelConfig,
                nodeMasterPrivKey,
                channelIndex,
                feeEstimator,
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

    member internal self.ChannelPrivKeys
        with get(): ChannelPrivKeys =
            self.Channel.ChannelPrivKeys

    member self.FundingTxId
        with get(): Option<TransactionIdentifier> =
            match self.Channel.State.Commitments with
            | Some commitments ->
                { DnlTxId = DotNetLightning.Utils.TxId commitments.FundingScriptCoin.Outpoint.Hash }
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
                                self.ChannelPrivKeys

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
