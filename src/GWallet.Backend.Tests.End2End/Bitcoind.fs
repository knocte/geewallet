namespace GWallet.Backend.Tests.End2End

open System
open System.IO

open Newtonsoft.Json
open NBitcoin
open DotNetLightning.Utils

open GWallet.Backend
open GWallet.Backend.FSharpUtil.UwpHacks

type Bitcoind = {
    DataDir: string
    RpcUser: string
    RpcPassword: string
    ProcessWrapper: ProcessWrapper
} with
    interface IDisposable with
        member self.Dispose() =
            self.ProcessWrapper.Process.Kill()
            self.ProcessWrapper.WaitForExit()
            Directory.Delete(self.DataDir, true)

    static member Start(): Async<Bitcoind> = async {
        let dataDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
        Directory.CreateDirectory dataDir |> ignore
        let rpcUser = Path.GetRandomFileName()
        let rpcPassword = Path.GetRandomFileName()
        let confPath = Path.Combine(dataDir, "bitcoin.conf")
        let fakeFeeRate = UtxoCoin.ElectrumClient.RegTestFakeFeeRate
        File.WriteAllText(
            confPath,
            SPrintF1
                "\
                txindex=1\n\
                printtoconsole=1\n\
                rpcallowip=127.0.0.1\n\
                zmqpubrawblock=tcp://127.0.0.1:28332\n\
                zmqpubrawtx=tcp://127.0.0.1:28333\n\
                fallbackfee=%f\n\
                [regtest]\n\
                rpcbind=127.0.0.1\n\
                rpcport=18554"
                fakeFeeRate
        )

        let processWrapper =
            ProcessWrapper.New
                "bitcoind"
                (SPrintF1 "-regtest -datadir=%s" dataDir)
                Map.empty
                false
        processWrapper.WaitForMessage (fun msg -> msg.EndsWith "init message: Done loading")

        do! Async.Sleep 2000

        return {
            DataDir = dataDir
            RpcUser = rpcUser
            RpcPassword = rpcPassword
            ProcessWrapper = processWrapper
        }
    }

    member self.GenerateBlocks (number: BlockHeightOffset32) (address: BitcoinAddress) =
        let bitcoinCli =
            ProcessWrapper.New
                "bitcoin-cli"
                (SPrintF3 "-regtest -datadir=%s generatetoaddress %i %s" self.DataDir number.Value (address.ToString()))
                Map.empty
                false
        bitcoinCli.WaitForExit()

    member this.GenerateBlocksToDummyAddress (number: BlockHeightOffset32) =
        let address =
            let key = new Key()
            key.PubKey.GetScriptAddress(Network.RegTest)
        this.GenerateBlocks number address

    member self.GetTxIdsInMempool(): list<TxId> =
        let bitcoinCli =
            ProcessWrapper.New
                "bitcoin-cli"
                (SPrintF1 "-regtest -datadir=%s getrawmempool" self.DataDir)
                Map.empty
                false
        let lines = bitcoinCli.ReadToEnd()
        let output = String.concat "\n" lines
        let txIdList = JsonConvert.DeserializeObject<list<string>> output
        List.map (fun (txIdString: string) -> TxId <| uint256 txIdString) txIdList

    member this.RpcAddr(): string =
        "127.0.0.1:18554"

    member this.RpcUrl(): string =
        SPrintF3 "http://%s:%s@%s" this.RpcUser this.RpcPassword (this.RpcAddr())

