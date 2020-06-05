namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.IO
open System.Net

open FSharp.Core

open Newtonsoft.Json
open NBitcoin
open DotNetLightning.Channel
open DotNetLightning.Utils
open DotNetLightning.Chain
open DotNetLightning.Crypto
open DotNetLightning.Transactions

open GWallet.Backend

type SerializedCommitmentSpec =
    {
        HTLCs: Map<HTLCId, DirectedHTLC>
        FeeRatePerKw: FeeRatePerKw
        ToLocal: LNMoney
        ToRemote: LNMoney
    }

type SerializedCommitments =
    {
        ChannelId: ChannelId
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

type CommitmentSpecConverter() =
    inherit JsonConverter<CommitmentSpec>()

    override __.ReadJson(reader: JsonReader, _: Type, _: CommitmentSpec, _: bool, serializer: JsonSerializer) =
        let serializedCommitmentSpec = serializer.Deserialize<SerializedCommitmentSpec> reader
        {
            CommitmentSpec.HTLCs = serializedCommitmentSpec.HTLCs
            FeeRatePerKw = serializedCommitmentSpec.FeeRatePerKw
            ToLocal = serializedCommitmentSpec.ToLocal
            ToRemote = serializedCommitmentSpec.ToRemote
        }

    override __.WriteJson(writer: JsonWriter, spec: CommitmentSpec, serializer: JsonSerializer) =
        let serializedCommitmentSpec =
            {
                SerializedCommitmentSpec.HTLCs = spec.HTLCs
                FeeRatePerKw = spec.FeeRatePerKw
                ToLocal = spec.ToLocal
                ToRemote = spec.ToRemote
            }
        serializer.Serialize(writer, serializedCommitmentSpec)

type CommitmentsJsonConverter() =
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
            ChannelId = serializedCommitments.ChannelId
            ChannelFlags = serializedCommitments.ChannelFlags
        }
        commitments

    override __.WriteJson(writer: JsonWriter, state: Commitments, serializer: JsonSerializer) =
        serializer.Serialize(writer, {
            ChannelId = state.ChannelId
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
        KeysRepoSeed: uint256
        Network: Network
        RemoteNodeId: PubKey
        ChanState: ChannelState
        AccountFileName: string
        CounterpartyIP: IPEndPoint
        // this is the amount of confirmations that the counterparty told us that the funding transaction needs
        MinSafeDepth: BlockHeightOffset32
    }
    static member LightningDir: Currency*DirectoryInfo =
        let currency = Settings.Currency
        let configDir = Config.GetConfigDir currency AccountKind.Normal
        currency, Path.Combine (configDir.FullName, "LN") |> DirectoryInfo
    static member ChannelFilePrefix = "chan"
    static member ChannelFileEnding = ".json"

    static member UIntToKeyRepo (channelKeysSeed: uint256): DefaultKeyRepository =
        let littleEndian = channelKeysSeed.ToBytes()
        DefaultKeyRepository (ExtKey littleEndian, 0)

    static member ExtractChannelNumber (channelFile: FileInfo): Option<FileInfo * int> =
        let fileName = channelFile.Name
        let withoutPrefix = fileName.Substring SerializedChannel.ChannelFilePrefix.Length
        let withoutEnding = withoutPrefix.Substring (0, withoutPrefix.Length - SerializedChannel.ChannelFileEnding.Length)
        match Int32.TryParse withoutEnding with
        | true, channelNumber ->
            Some (channelFile, channelNumber)
        | false, _ ->
            None

    static member LightningSerializerSettings: JsonSerializerSettings =
        let settings = JsonMarshalling.SerializerSettings
        let commitmentsConverter = CommitmentsJsonConverter()
        settings.Converters.Add commitmentsConverter
        settings

    member self.SaveSerializedChannel (fileName: string) =
        let json = Marshalling.SerializeCustom(self, SerializedChannel.LightningSerializerSettings, Marshalling.DefaultFormatting)
        let _,lnDir = SerializedChannel.LightningDir
        let filePath = Path.Combine (lnDir.FullName, fileName)
        if not lnDir.Exists then
            lnDir.Create()
        File.WriteAllText (filePath, json)
        ()

    static member Save (account: NormalAccount)
                       (chan: Channel)
                       (keysRepoSeed: uint256)
                       (ip: IPEndPoint)
                       (minSafeDepth: BlockHeightOffset32)
                           : string -> unit =
        let serializedChannel =
            {
                KeysRepoSeed = keysRepoSeed
                Network = chan.Network
                RemoteNodeId = chan.RemoteNodeId.Value
                ChanState = chan.State
                AccountFileName = account.AccountFile.Name
                CounterpartyIP = ip
                MinSafeDepth = minSafeDepth
            }
        serializedChannel.SaveSerializedChannel

    static member LoadSerializedChannel (fileName: string): SerializedChannel =
        let json = File.ReadAllText fileName
        Marshalling.DeserializeCustom<SerializedChannel> (json, SerializedChannel.LightningSerializerSettings)

    member self.ChannelFromSerialized (createChannel) (feeEstimator): Channel =
        let DummyProvideFundingTx (_ : IDestination * Money * FeeRatePerKw): Result<(FinalizedTx * TxOutIndex),string> =
            Result.Error "funding tx not needed cause channel already created"
        let keyRepo = SerializedChannel.UIntToKeyRepo self.KeysRepoSeed
        {
            createChannel
                keyRepo
                feeEstimator
                keyRepo.NodeSecret.PrivateKey
                DummyProvideFundingTx
                self.Network
                (NodeId self.RemoteNodeId)
            with State = self.ChanState
        }
