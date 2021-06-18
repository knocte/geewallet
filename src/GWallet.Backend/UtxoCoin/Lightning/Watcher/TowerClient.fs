namespace GWallet.Backend.UtxoCoin.Lightning.Watcher

open System.Net.Sockets

open StreamJsonRpc
open DotNetLightning.Utils
open DotNetLightning.Channel
open NBitcoin

open GWallet.Backend.UtxoCoin
open GWallet.Backend.UtxoCoin.Lightning
open GWallet.Backend
open GWallet.Backend.FSharpUtil.UwpHacks

type TowerUtxoCurrency =
    | Bitcoin
    | Litecoin

type TowerApiError =
    | UnsupportedCurrency

type TowerApiResponseOrError<'T> =
    | TowerApiResponse of 'T
    | TowerApiError of TowerApiError

type internal AddPunishmentTxRequest =
    {
        TransactionHex: string
        CommitmentTxHash: string
    }

type GetRewardAddressResponse =
    {
        RewardAddress: string
    }

type internal TowerClient =
    {
        TowerHost: string
        TowerPort: int
    }

    static member Default =
        {
            TowerHost = Config.DEFAULT_WATCHTOWER |> fst
            TowerPort = Config.DEFAULT_WATCHTOWER |> snd
        }

    member internal self.CreateAndSendPunishmentTx
        (perCommitmentSecret: PerCommitmentSecret)
        (savedChannelState: SavedChannelState)
        (localChannelPrivKeys: ChannelPrivKeys)
        (network: Network)
        (account: NormalUtxoAccount)
        (quietMode: bool)
        : Async<unit> =
        async {
            try
                let! rewardAddress = self.GetRewardAddress (account :> IAccount).Currency

                let! punishmentTx =
                    ForceCloseTransaction.CreatePunishmentTx
                        perCommitmentSecret
                        savedChannelState
                        localChannelPrivKeys
                        network
                        account
                        (rewardAddress |> Some)

                let towerRequest =
                    {
                        AddPunishmentTxRequest.TransactionHex = punishmentTx.ToHex()
                        CommitmentTxHash = savedChannelState.RemoteCommit.TxId.Value.ToString()
                    }

                do! self.AddPunishmentTx towerRequest
            with
            | ex ->
                if not quietMode then
                    raise <| FSharpUtil.ReRaise ex
        }


    member private self.AddPunishmentTx(request: AddPunishmentTxRequest): Async<unit> =
        async {
            use client = new TcpClient()

            do!
                client.ConnectAsync(self.TowerHost, self.TowerPort)
                |> Async.AwaitTask

            let mutable jsonRpc: JsonRpc = new JsonRpc(client.GetStream())
            jsonRpc.StartListening()

            return!
                jsonRpc.InvokeAsync("add_punishment_tx", request)
                |> Async.AwaitTask
        }

    member private self.GetRewardAddress(currency: Currency): Async<string> =
        async {
            use client = new TcpClient()

            do!
                client.ConnectAsync(self.TowerHost, self.TowerPort)
                |> Async.AwaitTask

            let mutable jsonRpc: JsonRpc = new JsonRpc(client.GetStream())
            jsonRpc.StartListening()

            let toTowerCurrency (currency: Currency) =
                match currency with
                | Currency.BTC -> TowerUtxoCurrency.Bitcoin
                | Currency.LTC -> TowerUtxoCurrency.Litecoin
                | _ -> failwith "only btc and ltc are supported on tower"

            let! response =
                jsonRpc.InvokeAsync<TowerApiResponseOrError<GetRewardAddressResponse>>("get_reward_address", toTowerCurrency currency)
                |> Async.AwaitTask

            return
                match response with
                | TowerApiResponse reward -> reward.RewardAddress
                | TowerApiError err -> failwith (SPrintF1 "Tower returned an error: %s" (err.ToString()))

        }
