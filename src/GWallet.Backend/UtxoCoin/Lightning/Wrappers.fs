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
    static member internal DefaultFundingTxMinimumDepth: BlockHeightOffset32 =
        BlockHeightOffset32 1u

    static member internal DefaultChannelOptions (currency: Currency) : Async<DotNetLightning.Channel.ChannelOptions> =
        async {
            let! feeEstimator = FeeEstimator.Create currency
            return
                {
                    FeeProportionalMillionths = 100u
                    MaxFeeRateMismatchRatio =
                        MonoHopUnidirectionalChannel.DefaultMaxFeeRateMismatchRatio
                    MaxClosingNegotiationIterations = 10
                    FeeEstimator = feeEstimator
                }
        }

    static member internal Create (account: UtxoCoin.NormalUtxoAccount)
                                  (nodeMasterPrivKey: NodeMasterPrivKey)
                                  (channelIndex: int)
                                  (savedChannelState: SavedChannelState)
                                  (remoteNextCommitInfo: Option<RemoteNextCommitInfo>)
                                  (negotiatingState: NegotiatingState)
                                  (commitments: Commitments)
                                      : Async<MonoHopUnidirectionalChannel> = async {
        let currency = (account :> IAccount).Currency
        let! channelOptions = MonoHopUnidirectionalChannel.DefaultChannelOptions (currency)
        let channelPrivKeys = nodeMasterPrivKey.ChannelPrivKeys channelIndex
        let channel = {
            SavedChannelState = savedChannelState
            ChannelOptions = channelOptions
            ChannelPrivKeys = channelPrivKeys
            NodeSecret = nodeMasterPrivKey.NodeSecret()
            RemoteNextCommitInfo = remoteNextCommitInfo
            NegotiatingState = negotiatingState
            Commitments = commitments
        }
        return { Channel = channel }
    }

    member internal self.RemoteNodeId
        with get(): NodeId = self.Channel.SavedChannelState.StaticChannelConfig.RemoteNodeId

    member internal self.Network
        with get(): Network = self.Channel.SavedChannelState.StaticChannelConfig.Network

    member self.ChannelId
        with get(): ChannelIdentifier =
            self.Channel.SavedChannelState.StaticChannelConfig.ChannelId()
            |> ChannelIdentifier.FromDnl 

    member internal self.ChannelPrivKeys
        with get(): ChannelPrivKeys =
            self.Channel.ChannelPrivKeys

    member internal self.FundingScriptCoin
        with get(): ScriptCoin =
            self.Channel.SavedChannelState.StaticChannelConfig.FundingScriptCoin

    member self.FundingTxId
        with get(): TransactionIdentifier = {
            DnlTxId = DotNetLightning.Utils.TxId self.FundingScriptCoin.Outpoint.Hash
        }

    member internal self.LocalParams (funding: Money)
                                         : LocalParams =
        Settings.GetLocalParams funding

    member internal self.Balance(): LNMoney =
        self.Channel.SavedChannelState.LocalCommit.Spec.ToLocal

    member internal self.SpendableBalance(): LNMoney =
        self.Channel.SpendableBalance()
