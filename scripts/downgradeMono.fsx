#!/usr/bin/env fsharpi

open System
open System.IO

#r "System.Configuration"
open System.Configuration
#load "InfraLib/Misc.fs"
#load "InfraLib/Process.fs"
#load "InfraLib/Unix.fs"
open FSX.Infrastructure
open Process

if Misc.GuessPlatform() <> Misc.Platform.Linux then
    failwith "This script is only supported on Linux"

let monoAptSource = FileInfo "/etc/apt/sources.list.d/mono-official-stable.list"
if monoAptSource.Exists then
    Unix.PurgeAllPackagesFromAptSource monoAptSource
    Unix.Sudo(sprintf "rm %s" monoAptSource.FullName)
    Unix.AptUpdate()
