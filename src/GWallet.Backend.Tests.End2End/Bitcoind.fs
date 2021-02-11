namespace GWallet.Backend.Tests.End2End

open System
open System.IO

open Newtonsoft.Json
open NBitcoin
open DotNetLightning.Utils

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

    static member Start(): Bitcoind =
        let dataDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
        Directory.CreateDirectory dataDir |> ignore
        let rpcUser = Path.GetRandomFileName()
        let rpcPassword = Path.GetRandomFileName()
        let confPath = Path.Combine(dataDir, "bitcoin.conf")
        File.WriteAllText(
            confPath,
            SPrintF2
                "\
                txindex=1\n\
                printtoconsole=1\n\
                rpcuser=%s\n\
                rpcpassword=%s\n\
                rpcallowip=127.0.0.1\n\
                zmqpubrawblock=tcp://127.0.0.1:28332\n\
                zmqpubrawtx=tcp://127.0.0.1:28333\n\
                fallbackfee=0.00001\n\
                [regtest]\n\
                rpcbind=127.0.0.1\n\
                rpcport=18554"
                rpcUser
                rpcPassword
        )

        let processWrapper =
            ProcessWrapper.New
                "bitcoind"
                (SPrintF1 "-regtest -datadir=%s" dataDir)
                Map.empty
                false
        processWrapper.WaitForMessage (fun msg -> msg.EndsWith "init message: Done loading")
        {
            DataDir = dataDir
            RpcUser = rpcUser
            RpcPassword = rpcPassword
            ProcessWrapper = processWrapper
        }

    member self.GenerateBlocks (number: BlockHeightOffset32) (address: BitcoinAddress) =
        let bitcoinCli =
            ProcessWrapper.New
                "bitcoin-cli"
                (SPrintF3 "-regtest -datadir=%s generatetoaddress %i %s" self.DataDir number.Value (address.ToString()))
                Map.empty
                false
        bitcoinCli.WaitForExit()

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

    member self.RpcUrl: string =
        SPrintF2 "http://%s:%s@127.0.0.1:18554" self.RpcUser self.RpcPassword
