﻿namespace GWallet.Backend.UtxoCoin.Lightning

open System.IO

open DotNetLightning.Channel
open NBitcoin

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.FSharpUtil.UwpHacks

type FundingBroadcastButNotLockedData =
    {
        Currency: Currency
        TxId: TransactionIdentifier
        MinimumDepth: uint32
    }
    member self.GetRemainingConfirmations(): Async<uint32> =
        async {
            let! confirmationCount =
                UtxoCoin.Server.Query
                    self.Currency
                    (UtxoCoin.QuerySettings.Default ServerSelectionMode.Fast)
                    (UtxoCoin.ElectrumClient.GetConfirmations (self.TxId.ToString()))
                    None
            if confirmationCount < self.MinimumDepth then
                let remainingConfirmations = self.MinimumDepth - confirmationCount
                return remainingConfirmations
            else
                return 0u
        }

type ChannelStatus =
    | FundingBroadcastButNotLocked of FundingBroadcastButNotLockedData
    | Closing
    | Closed
    | Active
    | Broken

type ChannelInfo =
    {
        ChannelId: ChannelIdentifier
        IsFunder: bool
        Balance: decimal
        SpendableBalance: decimal
        Capacity: decimal
        MaxBalance: decimal
        MinBalance: decimal
        FundingTxId: TransactionIdentifier
        Status: ChannelStatus
        Currency: Currency
    }
    static member internal FromSerializedChannel (serializedChannel: SerializedChannel)
                                                 (currency: Currency)
                                                     : ChannelInfo = {
        ChannelId = serializedChannel.ChannelId
        IsFunder = serializedChannel.IsFunder
        Balance = serializedChannel.Balance().ToMoney().ToUnit MoneyUnit.BTC
        SpendableBalance = serializedChannel.SpendableBalance().ToMoney().ToUnit MoneyUnit.BTC
        Capacity = serializedChannel.Capacity().ToUnit MoneyUnit.BTC
        MaxBalance = serializedChannel.MaxBalance().ToMoney().ToUnit MoneyUnit.BTC
        MinBalance = serializedChannel.MinBalance().ToMoney().ToUnit MoneyUnit.BTC
        FundingTxId = TransactionIdentifier.FromHash serializedChannel.Commitments.FundingScriptCoin.Outpoint.Hash
        Currency = currency
        Status =
            match serializedChannel.ChanState with
            | ChannelState.Negotiating _
            | ChannelState.Closing _ ->
                Closing
            | ChannelState.Closed _ ->
                Closed
            | ChannelState.Normal _ -> ChannelStatus.Active
            | ChannelState.WaitForFundingConfirmed waitForFundingConfirmedData ->
                let txId = TransactionIdentifier.FromHash waitForFundingConfirmedData.Commitments.FundingScriptCoin.Outpoint.Hash
                let minimumDepth = serializedChannel.MinSafeDepth.Value
                let fundingBroadcastButNotLockedData = {
                    Currency = currency
                    TxId = txId
                    MinimumDepth = minimumDepth
                }
                ChannelStatus.FundingBroadcastButNotLocked fundingBroadcastButNotLockedData
            | _ -> ChannelStatus.Broken
    }

type ChannelStore(account: NormalUtxoAccount) =
    static member ChannelFilePrefix = "chan-"
    static member ChannelFileEnding = ".json"

    member val Account = account
    member val Currency = (account :> IAccount).Currency

    member self.AccountDir: DirectoryInfo =
        Config.GetConfigDir self.Currency AccountKind.Normal

    member self.ChannelDir: DirectoryInfo =
        let subdirectory = SPrintF1 "%s-lightning" (self.Account :> BaseAccount).AccountFile.Name
        Path.Combine (self.AccountDir.FullName, subdirectory) |> DirectoryInfo

    member self.ListChannelIds(): seq<ChannelIdentifier> =
        let extractChannelId path: Option<ChannelIdentifier> =
            let fileName = Path.GetFileName path
            let withoutPrefix = fileName.Substring ChannelStore.ChannelFilePrefix.Length
            let withoutEnding =
                withoutPrefix.Substring(
                    0,
                    withoutPrefix.Length - ChannelStore.ChannelFileEnding.Length
                )
            ChannelIdentifier.Parse withoutEnding

        if self.ChannelDir.Exists then
            let files =
                Directory.GetFiles self.ChannelDir.FullName
            files |> Seq.choose extractChannelId
        else
            Seq.empty

    member self.ChannelFileName (channelId: ChannelIdentifier): string =
        Path.Combine(
            self.ChannelDir.FullName,
            SPrintF3
                "%s%s%s"
                ChannelStore.ChannelFilePrefix
                (channelId.ToString())
                ChannelStore.ChannelFileEnding
        )

    member internal self.LoadChannel (channelId: ChannelIdentifier): SerializedChannel =
        let fileName = self.ChannelFileName channelId
        let json = File.ReadAllText fileName
        Marshalling.DeserializeCustom<SerializedChannel> (
            json,
            SerializedChannel.LightningSerializerSettings self.Currency
        )

    member internal self.SaveChannel (serializedChannel: SerializedChannel) =
        let fileName = self.ChannelFileName serializedChannel.ChannelId
        let json =
            Marshalling.SerializeCustom(
                serializedChannel,
                SerializedChannel.LightningSerializerSettings self.Currency
            )
        if not self.ChannelDir.Exists then
            self.ChannelDir.Create()
        File.WriteAllText(fileName, json)

    member self.ChannelInfo (channelId: ChannelIdentifier): ChannelInfo =
        let serializedChannel = self.LoadChannel channelId
        ChannelInfo.FromSerializedChannel serializedChannel self.Currency

    member self.ListChannelInfos(): seq<ChannelInfo> = seq {
        for channelId in self.ListChannelIds() do
            let channelInfo = self.ChannelInfo channelId
            if channelInfo.Status <> ChannelStatus.Closing &&
               channelInfo.Status <> ChannelStatus.Closed then
                yield channelInfo
    }


module ChannelManager =
    // difference from fee estimation in UtxoCoinAccount.fs: this is for P2WSH
    let EstimateChannelOpeningFee (account: UtxoCoin.NormalUtxoAccount) (amount: TransferAmount) =
        let witScriptIdLength = 32
        // this dummy address is only used for fee estimation
        let nullScriptId = NBitcoin.WitScriptId (Array.zeroCreate witScriptIdLength)
        let network = UtxoCoin.Account.GetNetwork (account :> IAccount).Currency
        let dummyAddr = NBitcoin.BitcoinWitScriptAddress (nullScriptId, network)
        UtxoCoin.Account.EstimateFeeForDestination account amount dummyAddr

