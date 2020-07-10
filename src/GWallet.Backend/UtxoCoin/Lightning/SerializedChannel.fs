namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Net

open Newtonsoft.Json
open NBitcoin
open DotNetLightning.Channel
open DotNetLightning.Utils
open DotNetLightning.Crypto
open DotNetLightning.Transactions

open GWallet.Backend.FSharpUtil

type SerializedCommitmentSpec =
    {
        HTLCs: Map<HTLCId, DirectedHTLC>
        FeeRatePerKw: FeeRatePerKw
        ToLocal: LNMoney
        ToRemote: LNMoney
    }

type SerializedCommitments =
    {
        ChannelId: ChannelIdentifier
        ChannelFlags: uint8
        FundingScriptCoin: ScriptCoin
        LocalChanges: LocalChanges
        LocalCommit: LocalCommit
        LocalNextHTLCId: HTLCId
        LocalParams: LocalParams
        OriginChannels: Map<HTLCId, HTLCSource>
        RemoteChanges: RemoteChanges
        RemoteCommit: RemoteCommit
        RemoteNextCommitInfo: RemoteNextCommitInfo
        RemoteNextHTLCId: HTLCId
        RemoteParams: RemoteParams
        RemotePerCommitmentSecrets: ShaChain
    }

type private CommitmentsJsonConverter() =
    inherit JsonConverter<Commitments>()

    override __.ReadJson(reader: JsonReader, _: Type, _: Commitments, _: bool, serializer: JsonSerializer) =
        let serializedCommitments = serializer.Deserialize<SerializedCommitments> reader
        let commitments: Commitments = {
            RemotePerCommitmentSecrets = serializedCommitments.RemotePerCommitmentSecrets
            RemoteParams = serializedCommitments.RemoteParams
            RemoteNextHTLCId = serializedCommitments.RemoteNextHTLCId
            RemoteNextCommitInfo = serializedCommitments.RemoteNextCommitInfo
            RemoteCommit = serializedCommitments.RemoteCommit
            RemoteChanges = serializedCommitments.RemoteChanges
            OriginChannels = serializedCommitments.OriginChannels
            LocalParams = serializedCommitments.LocalParams
            LocalNextHTLCId = serializedCommitments.LocalNextHTLCId
            LocalCommit = serializedCommitments.LocalCommit
            LocalChanges = serializedCommitments.LocalChanges
            FundingScriptCoin = serializedCommitments.FundingScriptCoin
            ChannelId = serializedCommitments.ChannelId.DnlChannelId
            ChannelFlags = serializedCommitments.ChannelFlags
        }
        commitments

    override __.WriteJson(writer: JsonWriter, state: Commitments, serializer: JsonSerializer) =
        serializer.Serialize(writer, {
            ChannelId = ChannelIdentifier.FromDnl state.ChannelId
            ChannelFlags = state.ChannelFlags
            FundingScriptCoin = state.FundingScriptCoin
            LocalChanges = state.LocalChanges
            LocalCommit = state.LocalCommit
            LocalNextHTLCId = state.LocalNextHTLCId
            LocalParams = state.LocalParams
            OriginChannels = state.OriginChannels
            RemoteChanges = state.RemoteChanges
            RemoteCommit = state.RemoteCommit
            RemoteNextCommitInfo = state.RemoteNextCommitInfo
            RemoteNextHTLCId = state.RemoteNextHTLCId
            RemoteParams = state.RemoteParams
            RemotePerCommitmentSecrets = state.RemotePerCommitmentSecrets
        })

type SerializedChannel =
    {
        ChannelIndex: int
        Network: Network
        ChanState: ChannelState
        AccountFileName: string
        // FIXME: should store just RemoteNodeEndPoint instead of CounterpartyIP+RemoteNodeId?
        CounterpartyIP: IPEndPoint
        RemoteNodeId: NodeId
        // this is the amount of confirmations that the counterparty told us that the funding transaction needs
        MinSafeDepth: BlockHeightOffset32
    }
    static member LightningSerializerSettings: JsonSerializerSettings =
        let settings = JsonMarshalling.SerializerSettings
        let commitmentsConverter = CommitmentsJsonConverter()
        settings.Converters.Add commitmentsConverter
        settings

    member internal self.Commitments: Commitments =
        UnwrapOption
            self.ChanState.Commitments
            "A SerializedChannel is only created once a channel has started \
            being established and must therefore have an initial commitment"

    member self.IsFunder: bool =
        self.Commitments.LocalParams.IsFunder

    member internal self.Capacity(): Money =
        self.Commitments.FundingScriptCoin.Amount

    member internal self.Balance(): DotNetLightning.Utils.LNMoney =
        self.Commitments.LocalCommit.Spec.ToLocal

    member internal self.SpendableBalance(): DotNetLightning.Utils.LNMoney =
        self.Commitments.SpendableBalance()

    // How low the balance can go. A channel must maintain enough balance to
    // cover the channel reserve. The funder must also keep enough in the
    // channel to cover the closing fee.
    member internal this.MinBalance(): DotNetLightning.Utils.LNMoney =
        this.Balance() - this.SpendableBalance()

    // How high the balance can go. The fundee will only be able to receive up
    // to this amount before the funder no longer has enough funds to cover
    // the channel reserve and closing fee.
    member internal self.MaxBalance(): DotNetLightning.Utils.LNMoney =
        let capacity = LNMoney.FromMoney <| self.Capacity()
        let channelReserve =
            LNMoney.FromMoney self.Commitments.LocalParams.ChannelReserveSatoshis
        let fee =
            if self.IsFunder then
                let feeRate = self.Commitments.LocalCommit.Spec.FeeRatePerKw
                let weight = COMMITMENT_TX_BASE_WEIGHT
                LNMoney.FromMoney <| feeRate.ToFee weight
            else
                LNMoney.Zero
        capacity - channelReserve - fee

    member self.ChannelId: ChannelIdentifier =
        ChannelIdentifier.FromDnl self.Commitments.ChannelId

