#!/usr/bin/env fsharpi

open System
open System.IO
#r "System.Configuration"
open System.Configuration
#load "InfraLib/Misc.fs"
#load "InfraLib/Process.fs"
#load "InfraLib/Git.fs"
open FSX.Infrastructure
open Process

let rootDir = DirectoryInfo(Path.Combine(__SOURCE_DIRECTORY__, ".."))

let IsStable miniVersion =
    (int miniVersion % 2) = 0

let args = Misc.FsxArguments()
let suppliedVersion =
    if args.Length > 0 then
        if args.Length > 1 then
            Console.Error.WriteLine "Only one argument supported, not more"
            Environment.Exit 1
            failwith "Unreachable"
        else
            let full = Version(args.Head)
            if not (IsStable full.Build) then
                Console.Error.WriteLine "Mini-version (previous-to-last number, e.g. 2 in 0.1.2.3) should be an even (stable) number"
                Environment.Exit 2
                failwith "Unreachable"
            if full.Revision <> 0 then
                Console.Error.WriteLine "Revision number (last number, e.g. 3 in 0.1.2.3) should be zero (UWP restrictions...)"
                Environment.Exit 2
                failwith "Unreachable"
            Some full
    else
        None

let isReleaseManual = false

let filesToBumpMiniVersion: seq<string> =
    [
    ] :> seq<string>
let gitLabCiYml = ".gitlab-ci.yml"
let filesToBumpFullVersion: seq<string> =
    Seq.append filesToBumpMiniVersion [
        "src/GWallet.Backend/Properties/CommonAssemblyInfo.fs"
        "snap/snapcraft.yaml"
        ".github/workflows/ubuntu.yml"
        ".gitlab-ci.yml"
    ]
let filesToGitAdd: seq<string> =
    filesToBumpFullVersion

let Replace file fromStr toStr =
    let replaceScript = Path.Combine(rootDir.FullName, "scripts", "replace.fsx")
    let baseReplaceCommand =
        match Misc.GuessPlatform() with
        | Misc.Platform.Windows ->
            {
                Command = Path.Combine(rootDir.FullName, "scripts", "fsi.bat")
                Arguments = replaceScript
            }
        | _ ->
            {
                Command = replaceScript
                Arguments = String.Empty
            }
    let proc =
        {
            baseReplaceCommand with
                Arguments = sprintf "%s --file=%s %s %s"
                                baseReplaceCommand.Arguments
                                file
                                fromStr
                                toStr
        }
    Process.SafeExecute (proc, Echo.Off) |> ignore


let Bump(toStable: bool): Version*Version =
    let fullVersion = Misc.GetCurrentVersion(rootDir)
    let androidVersion = fullVersion.Build // 0.1.2.3 -> 2

    if toStable && IsStable androidVersion then
        failwith "bump script expects you to be in unstable version currently, but we found a stable"
    if (not toStable) && (not (IsStable androidVersion)) then
        failwith "sanity check failed, post-bump should happen in a stable version"

    let newFullVersion,newVersion =
        match suppliedVersion,toStable with
        | (Some full),true ->
            full,full.Build
        | _ ->
            let newVersion = androidVersion + 1
            let full = Version(sprintf "%i.%i.%i.%i"
                                       fullVersion.Major
                                       fullVersion.Minor
                                       newVersion
                                       fullVersion.Revision)
            full,newVersion

    let expiryFrom,expiryTo =
        if toStable then
            "50days","50years"
        else
            "50years","50days"

    Replace gitLabCiYml expiryFrom expiryTo

    for file in filesToBumpFullVersion do
        Replace file (fullVersion.ToString()) (newFullVersion.ToString())

    for file in filesToBumpFullVersion do
        Replace file
                (sprintf "versionCode=\\\"%s\\\"" (androidVersion.ToString()))
                (sprintf "versionCode=\\\"%s\\\"" (newVersion.ToString()))

    fullVersion,newFullVersion


let GitCommit (fullVersion: Version) (newFullVersion: Version) =
    for file in filesToGitAdd do
        let gitAdd =
            {
                Command = "git"
                Arguments = sprintf "add %s" file
            }
        Process.SafeExecute (gitAdd, Echo.Off) |> ignore

    let commitMessage = sprintf "Bump version: %s -> %s" (fullVersion.ToString()) (newFullVersion.ToString())
    let finalCommitMessage =
        if IsStable fullVersion.Build then
            sprintf "(Post)%s" commitMessage
        else
            commitMessage
    let gitCommit =
        {
            Command = "git"
            Arguments = sprintf "commit -m \"%s\"" finalCommitMessage
        }
    Process.SafeExecute (gitCommit,
                         Echo.Off) |> ignore

let GitTag (newFullVersion: Version) =
    if not (IsStable newFullVersion.Build) then
        failwith "something is wrong, this script should tag only even(stable) mini-versions, not odd(unstable) ones"

    let gitDeleteTag =
        {
            Command = "git"
            Arguments = sprintf "tag --delete %s" (newFullVersion.ToString())
        }
    Process.Execute (gitDeleteTag,
                     Echo.Off) |> ignore
    let gitCreateTag =
        {
            Command = "git"
            Arguments = sprintf "tag %s" (newFullVersion.ToString())
        }
    Process.SafeExecute (gitCreateTag,
                         Echo.Off) |> ignore

let GitDiff () =

    let gitDiff =
        {
            Command = "git"
            Arguments = "diff"
        }
    let gitDiffProc = Process.SafeExecute (gitDiff,
                                           Echo.Off)
    if gitDiffProc.Output.StdOut.Length > 0 then
        Console.Error.WriteLine "git status is not clean"
        Environment.Exit 1

let RunUpdateServers () =
    let makeCommand =
        match Misc.GuessPlatform() with
        | Misc.Platform.Windows ->
            "make.bat"
        | _ ->
            "make"
    let updateServersCmd =
        {
            Command = makeCommand
            Arguments = "update-servers"
        }
    Process.SafeExecute(updateServersCmd, Echo.OutputOnly) |> ignore
    let gitAddJson =
        {
            Command = "git"
            Arguments = "add src/GWallet.Backend/servers.json"
        }
    Process.SafeExecute (gitAddJson, Echo.Off) |> ignore

    let commitMessage = sprintf "Backend: update servers.json (pre-bump)"
    let gitCommit =
        {
            Command = "git"
            Arguments = sprintf "commit -m \"%s\"" commitMessage
        }
    Process.SafeExecute (gitCommit, Echo.Off) |> ignore
    GitDiff()


GitDiff()

Console.WriteLine "Bumping..."
RunUpdateServers()
let fullUnstableVersion,newFullStableVersion = Bump true
GitCommit fullUnstableVersion newFullStableVersion
GitTag newFullStableVersion

Console.WriteLine (sprintf "Version bumped to %s."
                           (newFullStableVersion.ToString()))

if isReleaseManual then
    Console.WriteLine "Release binaries now and press any key when you finish."
    Console.ReadKey true |> ignore

Console.WriteLine "Post-bumping..."
let fullStableVersion,newFullUnstableVersion = Bump false
GitCommit fullStableVersion newFullUnstableVersion

Console.WriteLine (sprintf "Version bumping finished. Remember to push via `git push <remote> <branch> %s`"
                           (newFullStableVersion.ToString()))
