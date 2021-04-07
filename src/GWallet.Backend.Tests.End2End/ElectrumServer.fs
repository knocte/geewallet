namespace GWallet.Backend.Tests.End2End

open System
open System.IO

open GWallet.Backend.FSharpUtil.UwpHacks

type ElectrumServer = {
    DbDir: string
    ProcessWrapper: ProcessWrapper
} with
    interface IDisposable with
        member self.Dispose() =
            self.ProcessWrapper.Process.Kill()
            self.ProcessWrapper.WaitForExit()
            Directory.Delete(self.DbDir, true)

    static member Start(bitcoind: Bitcoind): Async<ElectrumServer> = async {
        let dbDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
        Directory.CreateDirectory dbDir |> ignore
        let processWrapper =
            ProcessWrapper.New
                "electrs"
                (SPrintF3
                    "\
                    --db-dir %s \
                    --daemon-dir %s \
                    --network regtest \
                    --electrum-rpc-addr [::1]:50001 \
                    --daemon-rpc-addr %s \
                    "
                    dbDir
                    bitcoind.DataDir
                    (bitcoind.RpcAddr())
                )
                Map.empty
                false
        processWrapper.WaitForMessage (fun msg -> msg.Contains "Electrum Rust Server")

        do! Async.Sleep 5000

        return {
            DbDir = dbDir
            ProcessWrapper = processWrapper
        }
    }
