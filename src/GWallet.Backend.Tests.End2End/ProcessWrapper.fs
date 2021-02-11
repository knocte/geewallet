namespace GWallet.Backend.Tests.End2End

open System
open System.IO
open System.Threading
open System.Diagnostics
open System.Collections.Concurrent

open GWallet.Backend.FSharpUtil.UwpHacks

type ProcessWrapper = {
    Name: string
    Process: Process
    Queue: ConcurrentQueue<string>
    Semaphore: Semaphore
} with
    static member New (name: string)
                      (arguments: string)
                      (environment: Map<string, string>)
                      (isPython: bool)
                          : ProcessWrapper =

        let fileName =
            let environmentPath = System.Environment.GetEnvironmentVariable "PATH"
            let pathSeparator = Path.PathSeparator
            let paths = environmentPath.Split pathSeparator
            let isWin = Path.DirectorySeparatorChar = '\\'
            let exeName =
                if isWin then
                    name + if isPython then ".py" else ".exe"
                else
                    name
            let paths = [ for x in paths do yield Path.Combine(x, exeName) ]
            let matching = paths |> List.filter File.Exists
            match matching with
            | first :: _ -> first
            | _ ->
                failwith <|
                    SPrintF3
                        "Couldn't find %s in path, tried %A, these paths matched: %A"
                        exeName
                        [ for x in paths do yield (File.Exists x, x) ]
                        matching

        let queue = ConcurrentQueue()
        let semaphore = new Semaphore(0, Int32.MaxValue)
        let startInfo =
            ProcessStartInfo (
                UseShellExecute = false,
                FileName = fileName,
                Arguments = arguments,
                RedirectStandardOutput = true,
                RedirectStandardError = true
            )
        for kvp in environment do
            startInfo.Environment.[kvp.Key] <- kvp.Value
        let proc = new Process()
        proc.StartInfo <- startInfo
        let firstStreamEnded = ref false
        let outputHandler (_: obj) (args: DataReceivedEventArgs) =
            lock firstStreamEnded <| fun () ->
                match args.Data with
                | null ->
                    // We need to wait for both streams (stdout and stderr) to
                    // end. So output has ended and the process has exited
                    // after the second null.
                    if not !firstStreamEnded then
                        firstStreamEnded := true
                    else
                        Console.WriteLine(SPrintF2 "%s (%i) <exited>" name proc.Id)
                        semaphore.Release() |> ignore
                | text ->
                    Console.WriteLine(SPrintF3 "%s (%i): %s" name proc.Id text)
                    queue.Enqueue text
                    semaphore.Release() |> ignore
        proc.OutputDataReceived.AddHandler(DataReceivedEventHandler outputHandler)
        proc.ErrorDataReceived.AddHandler(DataReceivedEventHandler outputHandler)
        proc.EnableRaisingEvents <- true
        if not(proc.Start()) then
            failwith "failed to start process"
        AppDomain.CurrentDomain.ProcessExit.AddHandler(EventHandler (fun _ _ -> proc.Close()))
        proc.BeginOutputReadLine()
        proc.BeginErrorReadLine()
        {
            Name = name
            Process = proc
            Queue = queue
            Semaphore = semaphore
        }

    member self.WaitForMessage(msgFilter: string -> bool) =
        self.Semaphore.WaitOne() |> ignore
        let running, line = self.Queue.TryDequeue()
        if running then
            if msgFilter line then
                ()
            else
                self.WaitForMessage msgFilter
        else
            failwith (self.Name + " exited without outputting message")

    member self.WaitForExit() =
        self.Semaphore.WaitOne() |> ignore
        let running, _ = self.Queue.TryDequeue()
        if running then
            self.WaitForExit()

    member self.ReadToEnd(): list<string> =
        let rec fold (lines: list<string>) =
            self.Semaphore.WaitOne() |> ignore
            let running, line = self.Queue.TryDequeue()
            if running then
                fold <| List.append lines [line]
            else
                lines
        fold List.empty
