#!/usr/bin/env fsharpi

open System
open System.IO

#r "System.Configuration"
#load "InfraLib/Misc.fs"
#load "InfraLib/Process.fs"
#load "InfraLib/Git.fs"
open FSX.Infrastructure
open Process

let ConfigCommandCheck (commandNamesByOrderOfPreference: seq<string>) =
    let rec configCommandCheck currentCommandNamesQueue allCommands =
        match Seq.tryHead currentCommandNamesQueue with
        | Some currentCommand ->
            Console.Write (sprintf "checking for %s... " currentCommand)
            if not (Process.CommandWorksInShell currentCommand) then
                Console.WriteLine "not found"
                configCommandCheck (Seq.tail currentCommandNamesQueue) allCommands
            else
                Console.WriteLine "found"
                currentCommand
        | None ->
            Console.Error.WriteLine (sprintf "configure: error, please install %s" (String.Join(" or ", List.ofSeq allCommands)))
            Environment.Exit 1
            failwith "unreachable"
    configCommandCheck commandNamesByOrderOfPreference commandNamesByOrderOfPreference


let rootDir = DirectoryInfo(Path.Combine(__SOURCE_DIRECTORY__, ".."))

let initialConfigFile =
    match Misc.GuessPlatform() with
    | Misc.Platform.Windows ->
        // not using Mono anyway
        Map.empty

    | Misc.Platform.Mac ->
        ConfigCommandCheck ["mono"] |> ignore

        // unlikely that anyone uses old Mono versions in Mac, as it's easy to update (TODO: detect anyway)
        Map.empty

    | Misc.Platform.Linux ->
        ConfigCommandCheck ["mono"] |> ignore

        let pkgConfig = "pkg-config"
        ConfigCommandCheck [pkgConfig] |> ignore

        let pkgName = "mono"
        let stableVersionOfMono = Version("6.6")
        Console.Write (sprintf "checking for %s v%s... " pkgName (stableVersionOfMono.ToString()))

        let pkgConfigCmd = { Command = pkgConfig
                             Arguments = sprintf "--modversion %s" pkgName }
        let processResult = Process.Execute(pkgConfigCmd, Echo.Off)
        if processResult.ExitCode <> 0 then
            failwith "Mono was found but not detected by pkg-config?"

        let monoVersion = processResult.Output.StdOut.Trim()

        let currentMonoVersion = Version(monoVersion)
        if 1 = stableVersionOfMono.CompareTo currentMonoVersion then
            Console.WriteLine "not found"
            Console.Error.WriteLine (sprintf "configure: error, package requirements not met:")
            Console.Error.WriteLine (sprintf "Please upgrade %s version from %s to (at least) %s"
                                             pkgName
                                             (currentMonoVersion.ToString())
                                             (stableVersionOfMono.ToString()))
            Environment.Exit 1
        Console.WriteLine "found"
        Map.empty.Add("MonoPkgConfigVersion", monoVersion)

let targetsFileToExecuteNugetBeforeBuild = """<?xml version="1.0" encoding="utf-8"?>
<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
  <Import Project="$([MSBuild]::GetDirectoryNameOfFileAbove($(MSBuildThisFileDirectory), NuGet.Restore.targets))\NuGet.Restore.targets"
          Condition=" '$(NuGetRestoreImported)' != 'true' " />
</Project>
"""
File.WriteAllText(Path.Combine(rootDir.FullName, "before.gwallet.sln.targets"),
                  targetsFileToExecuteNugetBeforeBuild)

let buildTool =
    match Misc.GuessPlatform() with
    | Misc.Platform.Linux | Misc.Platform.Mac ->
        ConfigCommandCheck ["make"] |> ignore
        ConfigCommandCheck ["fsharpc"] |> ignore

        // needed by NuGet.Restore.targets & the "update-servers" Makefile target
        ConfigCommandCheck ["curl"]
            |> ignore

        ConfigCommandCheck [ "msbuild"; "xbuild" ]
    | Misc.Platform.Windows ->
        let programFiles = Environment.GetFolderPath Environment.SpecialFolder.ProgramFilesX86
        let msbuildPathPrefix = Path.Combine(programFiles, "Microsoft Visual Studio", "2019")
        let GetMsBuildPath vsEdition =
            Path.Combine(msbuildPathPrefix, vsEdition, "MSBuild", "Current", "Bin", "MSBuild.exe")

        // FIXME: we should use vscheck.exe
        ConfigCommandCheck
            [
                GetMsBuildPath "Community"
                GetMsBuildPath "Enterprise"
                GetMsBuildPath "BuildTools"
            ]


let prefix = DirectoryInfo(Misc.GatherOrGetDefaultPrefix(Misc.FsxArguments(), false, None))

if not (prefix.Exists) then
    let warning = sprintf "WARNING: prefix doesn't exist: %s" prefix.FullName
    Console.Error.WriteLine warning

let lines =
    let toConfigFileLine (keyValuePair: System.Collections.Generic.KeyValuePair<string,string>) =
        sprintf "%s=%s" keyValuePair.Key keyValuePair.Value

    initialConfigFile.Add("Prefix", prefix.FullName)
                     .Add("BuildTool", buildTool)
    |> Seq.map toConfigFileLine

let path = Path.Combine(__SOURCE_DIRECTORY__, "build.config")
File.AppendAllLines(path, lines |> Array.ofSeq)

let version = Misc.GetCurrentVersion(rootDir)

let repoInfo = Git.GetRepoInfo()

Console.WriteLine()
Console.WriteLine(sprintf
                      "\tConfiguration summary for gwallet %s %s"
                      (version.ToString()) repoInfo)
Console.WriteLine()
Console.WriteLine(sprintf
                      "\t* Installation prefix: %s"
                      prefix.FullName)
Console.WriteLine()

Console.WriteLine "Configuration succeeded, you can now run `make`"
