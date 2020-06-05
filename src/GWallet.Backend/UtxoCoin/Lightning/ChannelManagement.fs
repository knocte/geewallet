namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.IO
open System.Net.Sockets

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.FSharpUtil.UwpHacks

open NBitcoin
open DotNetLightning
open DotNetLightning.Peer
open DotNetLightning.Chain
open DotNetLightning.Utils
open DotNetLightning.Channel
open DotNetLightning.Serialize.Msgs

type PotentialChannel =
    internal
        {
            KeysSeed: uint256
            TemporaryId: ChannelId
        }

type ChannelEnvironment =
    internal
        {
            Account: NormalUtxoAccount
            NodeIdForResponder: NodeId
            KeyRepo: DefaultKeyRepository
        }

type IChannelToBeOpened =
    abstract member ConfirmationsRequired: uint32 with get

type OutgoingUnfundedChannel =
    internal
        {
            AcceptChannelMsg: AcceptChannelMsg
            Channel: Channel
            Peer: Peer
        }
    interface IChannelToBeOpened with
        member self.ConfirmationsRequired
            with get(): uint32 =
                self.AcceptChannelMsg.MinimumDepth.Value

type ChannelCreationDetails =
    {
        Client: TcpClient
        Password: Ref<string>

        // ideally we would mark the members below as 'internal' only, but: https://stackoverflow.com/q/62274013/544947
        ChannelInfo: PotentialChannel
        OutgoingUnfundedChannel: OutgoingUnfundedChannel
    }


type ChannelNotReadyReason =
    {
        CurrentConfirmations: uint32
        NeededConfirmations: uint32
    }

type ChannelStatus =
    // first element is always opening transaction id
    | UsableChannel of string
    | UnusableChannelWithReason of string * ChannelNotReadyReason

type internal ChannelDepthInfo =
    {
        ChannelId: ChannelId
        ConfirmationsCount: BlockHeightOffset32
        Channel: Channel
        Account: UtxoCoin.NormalUtxoAccount
        SerializedChannel: SerializedChannel
        FundingScriptCoin: ScriptCoin
    }

type internal ChannelDepth =
    | NotReady of ChannelNotReadyReason
    | DeepEnough of Async<ChannelCommand>

module ChannelManager =

    let ListSavedChannels (): seq<Currency * FileInfo * int> =
        let currency,lnDir = SerializedChannel.LightningDir
        if lnDir.Exists then
            let files =
                Directory.GetFiles
                    ((SerializedChannel.LightningDir.ToString()),
                     SerializedChannel.ChannelFilePrefix + "*" + SerializedChannel.ChannelFileEnding)
            files |> Seq.map FileInfo |> Seq.choose SerializedChannel.ExtractChannelNumber
            |> Seq.map (fun (file,channelId) -> currency, file, channelId)
        else
            Seq.empty

    let internal CreateChannelOptions (account: IAccount): ChannelOptions = {
        AnnounceChannel = false
        FeeProportionalMillionths = 100u
        MaxFeeRateMismatchRatio = 1.
        ShutdownScriptPubKey = Some (ScriptManager.CreatePayoutScript account)
    }

    let internal CreateChannelConfig (account: IAccount): ChannelConfig = {
        ChannelHandshakeConfig = Settings.HandshakeConfig
        PeerChannelConfigLimits = Settings.PeerLimits
        ChannelOptions = CreateChannelOptions account
    }

    let internal CreateChannel (account: IAccount) =
        let channelConfig = CreateChannelConfig account
        Channel.CreateCurried channelConfig

    let internal GetSeedAndRepo (random: Random): uint256 * DefaultKeyRepository * ChannelId =
        let channelKeysSeedBytes = Array.zeroCreate 32
        random.NextBytes channelKeysSeedBytes
        let channelKeysSeed = uint256 channelKeysSeedBytes
        let keyRepo = SerializedChannel.UIntToKeyRepo channelKeysSeed
        let temporaryChannelIdBytes: array<byte> = Array.zeroCreate 32
        random.NextBytes temporaryChannelIdBytes
        let temporaryChannelId =
            temporaryChannelIdBytes
            |> uint256
            |> ChannelId
        channelKeysSeed, keyRepo, temporaryChannelId

    let GenerateNewPotentialChannelDetails account (channelCounterpartyPubKey: PublicKey) =
        let random = Org.BouncyCastle.Security.SecureRandom () :> Random
        let channelKeysSeed, keyRepo, temporaryChannelId = GetSeedAndRepo random
        let pubKey = NBitcoin.PubKey (channelCounterpartyPubKey.ToString())
        let channelEnv = { Account = account; NodeIdForResponder = NodeId pubKey; KeyRepo = keyRepo }
        { KeysSeed = channelKeysSeed; TemporaryId = temporaryChannelId }, channelEnv

    let GetNewChannelFilename(): string =
        SerializedChannel.ChannelFilePrefix
            // this offset is the approximate time this feature was added (making filenames shorter)
            + (DateTimeOffset.Now.ToUnixTimeSeconds() - 1574212362L |> string)
            + SerializedChannel.ChannelFileEnding

    let EstimateChannelOpeningFee (account: UtxoCoin.NormalUtxoAccount) (amount: TransferAmount) =
        let witScriptIdLength = 32
        // this dummy address is only used for fee estimation
        let nullScriptId = NBitcoin.WitScriptId (Array.zeroCreate witScriptIdLength)
        let network = UtxoCoin.Account.GetNetwork (account :> IAccount).Currency
        let dummyAddr = NBitcoin.BitcoinWitScriptAddress (nullScriptId, network)
        UtxoCoin.Account.EstimateFeeForDestination account amount dummyAddr

    let internal JudgeDepth currency (details: ChannelDepthInfo)
                                         : ChannelDepth =
        if details.ConfirmationsCount >= details.SerializedChannel.MinSafeDepth then
            DeepEnough <|
                async {
                    let! txIndex, fundingBlockHeight =
                        ScriptManager.PositionInBlockFromScriptCoin
                            currency
                            details.ChannelId
                            details.FundingScriptCoin
                    let channelCommand =
                        ChannelCommand.ApplyFundingConfirmedOnBC
                            (fundingBlockHeight, txIndex, details.ConfirmationsCount)
                    return channelCommand
                }
        else
            NotReady {
                CurrentConfirmations = details.ConfirmationsCount.Value
                NeededConfirmations = details.SerializedChannel.MinSafeDepth.Value
            }

    let internal LoadChannelFetchingDepth currency (channelFile: FileInfo): Async<ChannelDepthInfo> =
        let serializedChannel = SerializedChannel.LoadSerializedChannel channelFile.FullName
        let accountFile = FileRepresentation.FromFile (FileInfo (serializedChannel.AccountFileName))

        let account = UtxoCoin.Account.GetAccountFromFile accountFile currency AccountKind.Normal
        // this downcast has to work because we passed AccountKind.Normal above! FIXME: still, I don't like this
        let normalAccount = account :?> NormalUtxoAccount

        async {
            let! feeEstimator = FeeEstimator.Create currency
            let feeEstimator = feeEstimator :> IFeeEstimator

            let channel =
                serializedChannel.ChannelFromSerialized
                    (CreateChannel account)
                    feeEstimator

            let (txId: ChannelId, fundingScriptCoin: ScriptCoin) =
                match channel.State with
                | ChannelState.WaitForFundingConfirmed waitForFundingConfirmedData ->
                    waitForFundingConfirmedData.ChannelId, waitForFundingConfirmedData.Commitments.FundingScriptCoin
                | ChannelState.Normal normalData ->
                    normalData.ChannelId, normalData.Commitments.FundingScriptCoin
                | _ as unknownState ->
                    // using failwith because this should never happen
                    failwith <| SPrintF1 "unexpected saved channel state: %s" (unknownState.GetType().Name)

            let txIdHex: string = txId.Value.ToString()
            let! confirmationsCount =
                async {
                    let! confirmations =
                        Server.Query
                            currency
                            (QuerySettings.Default ServerSelectionMode.Fast)
                            (ElectrumClient.GetConfirmations txIdHex)
                            None
                    return BlockHeightOffset32 confirmations
                }

            return {
                ChannelId = txId
                ConfirmationsCount = confirmationsCount
                Channel = channel
                Account = normalAccount
                SerializedChannel = serializedChannel
                FundingScriptCoin = fundingScriptCoin
            }
        }
