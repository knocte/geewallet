namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.IO

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Channel
open Newtonsoft.Json

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks


type CommitmentBreachData =
    {
        CommitmentNumber: UInt48
        PenaltyTx: string
    }

type ChannelBreachData =
    {
        ChannelId: ChannelIdentifier
        CommmitmentBreachData: List<CommitmentBreachData>
    }

    static member LightningSerializerSettings =
        let settings = JsonMarshalling.SerializerSettings
        let signatureConverter = NBitcoin.JsonConverters.SignatureJsonConverter ()

        settings.Converters.Add signatureConverter
        settings

    member internal self.BreachDataExists(commitmentNumber: CommitmentNumber) : bool =
        (self.CommmitmentBreachData |> List.tryFind (fun comm -> comm.CommitmentNumber = commitmentNumber.Index())).IsSome

    member internal self.GetBreachData(commitmentNumber: CommitmentNumber) : Option<CommitmentBreachData> =
        self.CommmitmentBreachData |> List.tryFind (fun comm -> comm.CommitmentNumber = commitmentNumber.Index())

    member internal self.InsertRevokedCommitment
                                        (perCommitmentSecret: PerCommitmentSecret)
                                        (commitments: Commitments)
                                        (localChannelPrivKeys: ChannelPrivKeys)
                                        (network: Network)
                                        (account: NormalUtxoAccount)
                                            : Async<ChannelBreachData> = async {

        let! punishmentTx =
            ForceCloseTransaction.CreatePunishmentTx perCommitmentSecret
                                                     commitments
                                                     localChannelPrivKeys
                                                     network
                                                     account

        let breachData : CommitmentBreachData =
            {
                PenaltyTx = punishmentTx.ToHex()
                CommitmentNumber = commitments.RemoteCommit.Index.Index()
            }

        return { self with CommmitmentBreachData = self.CommmitmentBreachData @ [ breachData ] }
    }

type internal BreachDataStore(account: NormalUtxoAccount) =
    static member BreachDataFilePrefix = "breach-"
    static member BreachDataFileEnding = ".json"

    member val Account = account
    member val Currency = (account :> IAccount).Currency

    member self.AccountDir: DirectoryInfo =
        Config.GetConfigDir self.Currency AccountKind.Normal

    member self.ChannelDir: DirectoryInfo =
        Path.Combine (self.AccountDir.FullName, Settings.ConfigDirName)
        |> DirectoryInfo

    member self.BreachDataFileName (channelId: ChannelIdentifier): string =
        Path.Combine(
            self.ChannelDir.FullName,
            SPrintF3
                "%s%s%s"
                BreachDataStore.BreachDataFilePrefix
                (channelId.ToString())
                BreachDataStore.BreachDataFileEnding
        )

    member internal self.LoadBreachData(channelId: ChannelIdentifier): ChannelBreachData =
        try
            let fileName = self.BreachDataFileName channelId
            let json = File.ReadAllText fileName
            Marshalling.DeserializeCustom<ChannelBreachData> (
                json,
                ChannelBreachData.LightningSerializerSettings
            )
        with
        | :? FileNotFoundException | :? DirectoryNotFoundException ->
            {
                ChannelBreachData.ChannelId = channelId
                ChannelBreachData.CommmitmentBreachData = []
            }

    // For now all lightning incoming messages are handled within a single thread, we don't need a lock here.
    member internal self.SaveBreachData (serializedBreachData: ChannelBreachData) =
        let fileName = self.BreachDataFileName serializedBreachData.ChannelId
        let json =
            Marshalling.SerializeCustom(
                serializedBreachData,
                ChannelBreachData.LightningSerializerSettings,
                Marshalling.DefaultFormatting
            )
        if not self.ChannelDir.Exists then
            self.ChannelDir.Create()
        File.WriteAllText(fileName, json)

