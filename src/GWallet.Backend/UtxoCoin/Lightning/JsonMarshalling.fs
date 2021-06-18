namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Net

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Crypto
open DotNetLightning.Serialization

open GWallet.Backend
open FSharpUtil

open Newtonsoft.Json

module JsonMarshalling =

    type internal ShutdownScriptPubKeyConverter() =
        inherit JsonConverter<ShutdownScriptPubKey>()

        override __.ReadJson(reader: JsonReader, _: Type, _: ShutdownScriptPubKey, _: bool, serializer: JsonSerializer) =
            let serializedScript = serializer.Deserialize<string> reader
            let script = Script.FromHex serializedScript
            let shutdownScriptPubKeyRes = ShutdownScriptPubKey.TryFromScript script
            let shutdownScriptPubKey = UnwrapResult shutdownScriptPubKeyRes "malformed shutdown script in wallet"
            shutdownScriptPubKey

        override __.WriteJson(writer: JsonWriter, state: ShutdownScriptPubKey, serializer: JsonSerializer) =
            let script = state.ScriptPubKey().ToHex()
            serializer.Serialize(writer, script)

    type internal CommitmentNumberConverter() =
        inherit JsonConverter<CommitmentNumber>()

        override __.ReadJson(reader: JsonReader, _: Type, _: CommitmentNumber, _: bool, serializer: JsonSerializer) =
            let serializedCommitmentNumber = serializer.Deserialize<uint64> reader
            CommitmentNumber <| (UInt48.MaxValue - UInt48.FromUInt64 serializedCommitmentNumber)

        override __.WriteJson(writer: JsonWriter, state: CommitmentNumber, serializer: JsonSerializer) =
            let serializedCommitmentNumber: uint64 = (UInt48.MaxValue - state.Index()).UInt64
            serializer.Serialize(writer, serializedCommitmentNumber)

    type internal PerCommitmentSecretStoreConverter() =
        inherit JsonConverter<PerCommitmentSecretStore>()

        override __.ReadJson(reader: JsonReader, _: Type, _: PerCommitmentSecretStore, _: bool, serializer: JsonSerializer) =
            let keys = serializer.Deserialize<List<CommitmentNumber * PerCommitmentSecret>> reader
            PerCommitmentSecretStore.FromSecrets keys

        override __.WriteJson(writer: JsonWriter, state: PerCommitmentSecretStore, serializer: JsonSerializer) =
            let keys: List<CommitmentNumber * PerCommitmentSecret> = state.Secrets
            serializer.Serialize(writer, keys)

    type internal ChannelIdentifierConverter() =
        inherit JsonConverter<ChannelIdentifier>()

        override __.ReadJson(reader: JsonReader, _: Type, _: ChannelIdentifier, _: bool, serializer: JsonSerializer) =
            let serializedChannelId = serializer.Deserialize<string> reader
            serializedChannelId
            |> NBitcoin.uint256
            |> DotNetLightning.Utils.ChannelId
            |> ChannelIdentifier.FromDnl

        override __.WriteJson(writer: JsonWriter, state: ChannelIdentifier, serializer: JsonSerializer) =
            let serializedChannelId: string = state.DnlChannelId.Value.ToString()
            serializer.Serialize(writer, serializedChannelId)

    type internal FeatureBitJsonConverter() =
        inherit JsonConverter<FeatureBits>()

        override __.ReadJson(reader: JsonReader, _: Type, _: FeatureBits, _: bool, serializer: JsonSerializer): FeatureBits =
            let serializedFeatureBit = serializer.Deserialize<string> reader
            UnwrapResult (FeatureBits.TryParse serializedFeatureBit) "error decoding feature bit"

        override __.WriteJson(writer: JsonWriter, state: FeatureBits, serializer: JsonSerializer) =
            serializer.Serialize(writer, state.ToString())

    type internal IPAddressJsonConverter() =
        inherit JsonConverter<IPAddress>()

        override __.ReadJson(reader: JsonReader, _: Type, _: IPAddress, _: bool, serializer: JsonSerializer) =
            let serializedIPAddress = serializer.Deserialize<string> reader
            IPAddress.Parse serializedIPAddress

        override __.WriteJson(writer: JsonWriter, state: IPAddress, serializer: JsonSerializer) =
            serializer.Serialize(writer, state.ToString())

    type internal IPEndPointJsonConverter() =
        inherit JsonConverter<IPEndPoint>()

        override __.ReadJson(reader: JsonReader, _: Type, _: IPEndPoint, _: bool, serializer: JsonSerializer) =
            assert (reader.TokenType = JsonToken.StartArray)
            reader.Read() |> ignore
            let ip = serializer.Deserialize<IPAddress> reader
            reader.Read() |> ignore
            let port = serializer.Deserialize<int32> reader
            reader.Read() |> ignore
            IPEndPoint (ip, port)

        override __.WriteJson(writer: JsonWriter, state: IPEndPoint, serializer: JsonSerializer) =
            writer.WriteStartArray()
            serializer.Serialize(writer, state.Address)
            serializer.Serialize(writer, state.Port)
            writer.WriteEndArray()

    let internal SerializerSettings: JsonSerializerSettings =
        let settings = Marshalling.DefaultSettings ()
        let shutdownScriptPubKeyConverter = ShutdownScriptPubKeyConverter()
        let ipAddressConverter = IPAddressJsonConverter()
        let ipEndPointConverter = IPEndPointJsonConverter()
        let featureBitConverter = FeatureBitJsonConverter()
        let channelIdentifierConverter = ChannelIdentifierConverter()
        let commitmentNumberConverter = CommitmentNumberConverter()
        let perCommitmentSecretStoreConverter = PerCommitmentSecretStoreConverter()

        settings.Converters.Add shutdownScriptPubKeyConverter
        settings.Converters.Add ipAddressConverter
        settings.Converters.Add ipEndPointConverter
        settings.Converters.Add featureBitConverter
        settings.Converters.Add channelIdentifierConverter
        settings.Converters.Add commitmentNumberConverter
        settings.Converters.Add perCommitmentSecretStoreConverter

        NBitcoin.JsonConverters.Serializer.RegisterFrontConverters settings
        settings

