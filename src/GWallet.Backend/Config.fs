namespace GWallet.Backend

open System
open System.IO
open System.Linq
open System.Reflection

open Xamarin.Essentials

open GWallet.Backend.FSharpUtil.UwpHacks

// TODO: make internal when tests don't depend on this anymore
module Config =

    // we might want to test with TestNet at some point, so this below is the key:
    // (but we would need to get a seed list of testnet electrum servers, and testnet(/ropsten/rinkeby?), first...)
    let BitcoinNet = NBitcoin.Network.Main
    let LitecoinNet = NBitcoin.Altcoins.Litecoin.Instance.Mainnet
    let EtcNet = Nethereum.Signer.Chain.ClassicMainNet
    let EthNet = Nethereum.Signer.Chain.MainNet

    // https://github.com/Nethereum/Nethereum/issues/509
    let EthTokenEstimationCouldBeBuggyAsInNotAccurate = true

    let internal DebugLog =
#if DEBUG
        true
#else
        false
#endif

    // NOTE: enabling this might look confusing because it only works for non-cache
    //       balances, so you might find discrepancies (e.g. the donut-chart-view)
    let internal NoNetworkBalanceForDebuggingPurposes = false

    let IsWindowsPlatform() =
        Path.DirectorySeparatorChar = '\\'

    let IsMacPlatform() =
        let macDirs = [ "/Applications"; "/System"; "/Users"; "/Volumes" ]
        match Environment.OSVersion.Platform with
        | PlatformID.MacOSX ->
            true
        | PlatformID.Unix ->
            if macDirs.All(fun dir -> Directory.Exists dir) then
                if not (DeviceInfo.Platform.Equals DevicePlatform.iOS) then
                    true
                else
                    false
            else
                false
        | _ ->
            false

    let GetMonoVersion(): Option<Version> =
        FSharpUtil.option {
            // this gives None on MS.NET (e.g. UWP/WPF)
            let! monoRuntime = Type.GetType "Mono.Runtime" |> Option.ofObj
            // this gives None on Mono Android/iOS/macOS
            let! displayName =
                monoRuntime.GetMethod("GetDisplayName", BindingFlags.NonPublic ||| BindingFlags.Static) |> Option.ofObj
                // example: 5.12.0.309 (2018-02/39d89a335c8 Thu Sep 27 06:54:53 EDT 2018)
            let fullVersion = displayName.Invoke(null, null) :?> string
            let simpleVersion = fullVersion.Substring(0, fullVersion.IndexOf(' ')) |> Version
            return simpleVersion
        }

    // FIXME: make FaultTolerantParallelClient accept funcs that receive this as an arg, maybe 2x-ing it when a full
    //        round of failures has happened, as in, all servers failed
    let internal DEFAULT_NETWORK_TIMEOUT = TimeSpan.FromSeconds 30.0
    let internal DEFAULT_NETWORK_CONNECT_TIMEOUT = TimeSpan.FromSeconds 5.0

    let internal NUMBER_OF_RETRIES_TO_SAME_SERVERS = 3u

    let internal GetConfigDirForThisProgram() =
        let configPath = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)
        let configDir = DirectoryInfo(Path.Combine(configPath, "gwallet"))
        if not configDir.Exists then
            configDir.Create()
        configDir

    let internal GetCacheDir() =
        let configPath = GetConfigDirForThisProgram().FullName
        let configDir = DirectoryInfo(Path.Combine(configPath, "cache"))
        if not configDir.Exists then
            configDir.Create()
        configDir

    let private GetConfigDirForAccounts() =
        let configPath = GetConfigDirForThisProgram().FullName
        let configDir = DirectoryInfo(Path.Combine(configPath, "accounts"))
        if not configDir.Exists then
            configDir.Create()
        configDir

    let internal GetConfigDir (currency: Currency) (accountKind: AccountKind) =
        let accountConfigDir = GetConfigDirForAccounts().FullName

        let baseConfigDir =
            match accountKind with
            | AccountKind.Normal ->
                accountConfigDir
            | AccountKind.ReadOnly ->
                Path.Combine(accountConfigDir, "readonly")
            | AccountKind.Archived ->
                Path.Combine(accountConfigDir, "archived")

        let configDir = Path.Combine(baseConfigDir, currency.ToString()) |> DirectoryInfo
        if not configDir.Exists then
            configDir.Create()
        configDir

    // In case a new token was added it will not have a config for an existing user
    // we copy the eth configs to the new tokens config directory
    let PropagateEthAccountInfoToMissingTokensAccounts() =
        for accountKind in (AccountKind.All()) do
            let ethConfigDir = GetConfigDir Currency.ETH accountKind
            for token in Currency.GetAll() do
                if token.IsEthToken() then
                    let tokenConfigDir = GetConfigDir token accountKind
                    for ethAccountFilePath in Directory.GetFiles ethConfigDir.FullName do
                        let newPath = ethAccountFilePath.Replace(ethConfigDir.FullName, tokenConfigDir.FullName)
                        if not (File.Exists newPath) then
                            File.Copy(ethAccountFilePath, newPath)

    let GetAccountFiles (currencies: seq<Currency>) (accountKind: AccountKind): seq<FileRepresentation> =
        seq {
            for currency in currencies do
                for filePath in Directory.GetFiles (GetConfigDir currency accountKind).FullName do
                    yield FileRepresentation.FromFile (FileInfo(filePath))
        }

    let private GetFile (currency: Currency) (account: BaseAccount): FileInfo =
        let configDir, fileName = GetConfigDir currency account.Kind, account.AccountFile.Name
        Path.Combine(configDir.FullName, fileName) |> FileInfo

    let AddAccount (conceptAccount: ConceptAccount) (accountKind: AccountKind): FileRepresentation =
        let configDir = GetConfigDir conceptAccount.Currency accountKind
        let newAccountFile = Path.Combine(configDir.FullName, conceptAccount.FileRepresentation.Name) |> FileInfo
        if newAccountFile.Exists then
            raise AccountAlreadyAdded
        File.WriteAllText(newAccountFile.FullName, conceptAccount.FileRepresentation.Content())

        {
            Name = Path.GetFileName newAccountFile.FullName
            Content = fun _ -> File.ReadAllText newAccountFile.FullName
        }

    let public Wipe (): unit =
        let configDirForAccounts = GetConfigDirForAccounts()
        Directory.Delete(configDirForAccounts.FullName, true)

    // we don't expose this as public because we don't want to allow removing archived accounts
    let private RemoveAccount (account: BaseAccount): unit =
        let configFile = GetFile (account:>IAccount).Currency account
        if not configFile.Exists then
            failwith <| SPrintF1 "File %s doesn't exist. Please report this issue." configFile.FullName
        else
            configFile.Delete()

    let RemoveNormalAccount (account: NormalAccount): unit =
        RemoveAccount account

    let RemoveReadOnlyAccount (account: ReadOnlyAccount): unit =
        RemoveAccount account

    let ExtractEmbeddedResourceFileContents resourceName =
        let assembly = Assembly.GetExecutingAssembly()
        use stream = assembly.GetManifestResourceStream resourceName
        if (stream = null) then
            failwith <| SPrintF1 "Embedded resource %s not found" resourceName
        use reader = new StreamReader(stream)
        reader.ReadToEnd()
