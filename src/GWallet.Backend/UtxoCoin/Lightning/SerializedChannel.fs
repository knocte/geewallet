namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Net

open Newtonsoft.Json
open NBitcoin
open DotNetLightning.Channel
open DotNetLightning.Utils
open DotNetLightning.Crypto
open DotNetLightning.Transactions
open DotNetLightning.Serialization.Msgs

open GWallet.Backend.UtxoCoin
open GWallet.Backend.FSharpUtil

type SerializedCommitmentSpec =
    {
        OutgoingHTLCs: Map<HTLCId, UpdateAddHTLCMsg>
        IncomingHTLCs: Map<HTLCId, UpdateAddHTLCMsg>
        FeeRatePerKw: FeeRatePerKw
        ToLocal: LNMoney
        ToRemote: LNMoney
    }

type SerializedCommitments =
    {
        ProposedLocalChanges: list<IUpdateMsg>
        ProposedRemoteChanges: list<IUpdateMsg>
        LocalNextHTLCId: HTLCId
        RemoteNextHTLCId: HTLCId
        OriginChannels: Map<HTLCId, HTLCSource>
    }

type private CommitmentsJsonConverter() =
    inherit JsonConverter<Commitments>()

    override __.ReadJson(reader: JsonReader, _: Type, _: Commitments, _: bool, serializer: JsonSerializer) =
        let serializedCommitments = serializer.Deserialize<SerializedCommitments> reader
        let commitments: Commitments = {
            ProposedLocalChanges = serializedCommitments.ProposedLocalChanges
            ProposedRemoteChanges = serializedCommitments.ProposedRemoteChanges
            LocalNextHTLCId = serializedCommitments.LocalNextHTLCId
            RemoteNextHTLCId = serializedCommitments.RemoteNextHTLCId
            OriginChannels = serializedCommitments.OriginChannels
        }
        commitments

    override __.WriteJson(writer: JsonWriter, state: Commitments, serializer: JsonSerializer) =
        serializer.Serialize(writer, {
            ProposedLocalChanges = state.ProposedLocalChanges
            ProposedRemoteChanges = state.ProposedRemoteChanges
            LocalNextHTLCId = state.LocalNextHTLCId
            RemoteNextHTLCId = state.RemoteNextHTLCId
            OriginChannels = state.OriginChannels
        })

type SerializedChannel =
    {
        ChannelIndex: int
        SavedChannelState: SavedChannelState
        Commitments: Commitments
        RemoteNextCommitInfo: Option<RemoteNextCommitInfo>
        NegotiatingState: NegotiatingState
        AccountFileName: string
        // FIXME: should store just RemoteNodeEndPoint instead of CounterpartyIP+RemoteNodeId?
        CounterpartyIP: IPEndPoint
        LocalForceCloseSpendingTxOpt: Option<string>
        LocalChannelPubKeys: ChannelPubKeys
    }
    static member LightningSerializerSettings currency: JsonSerializerSettings =
        let settings = JsonMarshalling.SerializerSettings

        let commitmentsConverter = CommitmentsJsonConverter()
        settings.Converters.Add commitmentsConverter

        let psbtConverter = NBitcoin.JsonConverters.PSBTJsonConverter (Account.GetNetwork currency)
        settings.Converters.Add psbtConverter

        settings

    member internal self.FundingScriptCoin() =
        self.SavedChannelState.StaticChannelConfig.FundingScriptCoin

    member internal self.MinDepth() =
        self.SavedChannelState.StaticChannelConfig.FundingTxMinimumDepth

    member internal self.IsFunder(): bool =
        self.SavedChannelState.StaticChannelConfig.IsFunder

    member internal self.Capacity(): Money =
        self.FundingScriptCoin().Amount

    member internal self.Balance(): DotNetLightning.Utils.LNMoney =
        self.SavedChannelState.LocalCommit.Spec.ToLocal

    member internal self.SpendableBalance(): LNMoney =
        Channel.SpendableBalanceFromParts self.SavedChannelState
                                          self.RemoteNextCommitInfo
                                          self.Commitments

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
            LNMoney.FromMoney self.SavedChannelState.StaticChannelConfig.LocalParams.ChannelReserveSatoshis
        let fee =
            if self.IsFunder() then
                let feeRate = self.SavedChannelState.LocalCommit.Spec.FeeRatePerKw
                let weight = COMMITMENT_TX_BASE_WEIGHT
                LNMoney.FromMoney <| feeRate.CalculateFeeFromWeight weight
            else
                LNMoney.Zero
        capacity - channelReserve - fee

    member internal self.ChannelId (): ChannelIdentifier =
        self.SavedChannelState.StaticChannelConfig.ChannelId()
        |> ChannelIdentifier.FromDnl 

