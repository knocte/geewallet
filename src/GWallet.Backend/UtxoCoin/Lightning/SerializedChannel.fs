namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Net

open Newtonsoft.Json
open NBitcoin
open DotNetLightning.Channel
open DotNetLightning.Utils
open DotNetLightning.Crypto
open DotNetLightning.Transactions

open GWallet.Backend.UtxoCoin
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
        IsFunder: bool
        ChannelFlags: uint8
        FundingScriptCoin: ScriptCoin
        LocalChanges: LocalChanges
        LocalCommit: LocalCommit
        LocalNextHTLCId: HTLCId
        LocalParams: LocalParams
        OriginChannels: Map<HTLCId, HTLCSource>
        RemoteChanges: RemoteChanges
        RemoteCommit: RemoteCommit
        RemoteNextHTLCId: HTLCId
        RemoteParams: RemoteParams
        RemotePerCommitmentSecrets: PerCommitmentSecretStore
        RemoteChannelPubKeys: ChannelPubKeys
    }

type private CommitmentsJsonConverter() =
    inherit JsonConverter<Commitments>()

    override __.ReadJson(reader: JsonReader, _: Type, _: Commitments, _: bool, serializer: JsonSerializer) =
        let serializedCommitments = serializer.Deserialize<SerializedCommitments> reader
        let commitments: Commitments = {
            RemotePerCommitmentSecrets = serializedCommitments.RemotePerCommitmentSecrets
            RemoteParams = serializedCommitments.RemoteParams
            RemoteNextHTLCId = serializedCommitments.RemoteNextHTLCId
            RemoteCommit = serializedCommitments.RemoteCommit
            RemoteChanges = serializedCommitments.RemoteChanges
            OriginChannels = serializedCommitments.OriginChannels
            LocalParams = serializedCommitments.LocalParams
            LocalNextHTLCId = serializedCommitments.LocalNextHTLCId
            LocalCommit = serializedCommitments.LocalCommit
            LocalChanges = serializedCommitments.LocalChanges
            FundingScriptCoin = serializedCommitments.FundingScriptCoin
            ChannelFlags = serializedCommitments.ChannelFlags
            IsFunder = serializedCommitments.IsFunder
            RemoteChannelPubKeys = serializedCommitments.RemoteChannelPubKeys
        }
        commitments

    override __.WriteJson(writer: JsonWriter, state: Commitments, serializer: JsonSerializer) =
        serializer.Serialize(writer, {
            ChannelFlags = state.ChannelFlags
            FundingScriptCoin = state.FundingScriptCoin
            LocalChanges = state.LocalChanges
            LocalCommit = state.LocalCommit
            LocalNextHTLCId = state.LocalNextHTLCId
            LocalParams = state.LocalParams
            OriginChannels = state.OriginChannels
            RemoteChanges = state.RemoteChanges
            RemoteCommit = state.RemoteCommit
            RemoteNextHTLCId = state.RemoteNextHTLCId
            RemoteParams = state.RemoteParams
            RemotePerCommitmentSecrets = state.RemotePerCommitmentSecrets
            RemoteChannelPubKeys = state.RemoteChannelPubKeys
            IsFunder = state.IsFunder
        })

type SerializedChannel =
    {
        ChannelIndex: int
        Network: Network
        ChanState: ChannelState
        Commitments: Commitments
        AccountFileName: string
        // FIXME: should store just RemoteNodeEndPoint instead of CounterpartyIP+RemoteNodeId?
        CounterpartyIP: IPEndPoint
        RemoteNodeId: NodeId
        LocalForceCloseSpendingTxOpt: Option<string>
        // this is the amount of confirmations that the counterparty told us that the funding transaction needs
        MinSafeDepth: BlockHeightOffset32
        LocalChannelPubKeys: ChannelPubKeys
    }
    static member LightningSerializerSettings currency: JsonSerializerSettings =
        let settings = JsonMarshalling.SerializerSettings

        let commitmentsConverter = CommitmentsJsonConverter()
        settings.Converters.Add commitmentsConverter

        let psbtConverter = NBitcoin.JsonConverters.PSBTJsonConverter (Account.GetNetwork currency)
        settings.Converters.Add psbtConverter

        settings

    member internal self.IsFunder(): bool =
        self.Commitments.IsFunder

    member internal self.Capacity(): Money =
        self.Commitments.FundingScriptCoin.Amount

    member internal self.Balance(): DotNetLightning.Utils.LNMoney =
        self.Commitments.LocalCommit.Spec.ToLocal

    member internal self.SpendableBalance(): LNMoney =
        let remoteNextCommitInfoOpt =
            match self.ChanState with
            | ChannelState.WaitForFundingConfirmed _ -> None
            | ChannelState.WaitForFundingLocked _ -> None
            | ChannelState.Normal data -> Some data.RemoteNextCommitInfo
            | ChannelState.Shutdown data -> Some data.RemoteNextCommitInfo
            | ChannelState.Negotiating data -> Some data.RemoteNextCommitInfo
            | ChannelState.Closing data -> Some data.RemoteNextCommitInfo
        self.Commitments.SpendableBalance remoteNextCommitInfoOpt

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
            if self.IsFunder() then
                let feeRate = self.Commitments.LocalCommit.Spec.FeeRatePerKw
                let weight = COMMITMENT_TX_BASE_WEIGHT
                LNMoney.FromMoney <| feeRate.CalculateFeeFromWeight weight
            else
                LNMoney.Zero
        capacity - channelReserve - fee

    member internal self.ChannelId (): ChannelIdentifier =
        self.Commitments.ChannelId()
        |> ChannelIdentifier.FromDnl 

