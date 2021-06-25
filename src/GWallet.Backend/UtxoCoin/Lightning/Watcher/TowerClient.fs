namespace GWallet.Backend.UtxoCoin.Lightning.Watcher

open System.Net.Sockets

open StreamJsonRpc
open DotNetLightning.Utils
open DotNetLightning.Channel
open NBitcoin

open GWallet.Backend.UtxoCoin
open GWallet.Backend.UtxoCoin.Lightning
open GWallet.Backend

type internal AddPunishmentTxRequest =
    {
        TransactionHex: string
        CommitmentTxHash: string
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
        (commitments: Commitments)
        (localChannelPrivKeys: ChannelPrivKeys)
        (network: Network)
        (account: NormalUtxoAccount)
        (quietMode: bool)
        : Async<unit> =
        async {
            try
                let! rewardAddress = self.GetRewardAddress()

                let! punishmentTx =
                    ForceCloseTransaction.CreatePunishmentTx
                        perCommitmentSecret
                        commitments
                        localChannelPrivKeys
                        network
                        account
                        (rewardAddress |> Some)

                let towerRequest =
                    {
                        AddPunishmentTxRequest.TransactionHex = punishmentTx.ToHex()
                        CommitmentTxHash = commitments.RemoteCommit.TxId.Value.ToString()
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

    member private self.GetRewardAddress(): Async<string> =
        async {
            use client = new TcpClient()

            do!
                client.ConnectAsync(self.TowerHost, self.TowerPort)
                |> Async.AwaitTask

            let mutable jsonRpc: JsonRpc = new JsonRpc(client.GetStream())
            jsonRpc.StartListening()

            return!
                jsonRpc.InvokeAsync<string>("get_reward_address")
                |> Async.AwaitTask
        }
