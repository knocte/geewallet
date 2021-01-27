namespace GWallet.Backend.UtxoCoin

open System
open System.Net

open FSharp.Data
open FSharp.Data.JsonExtensions
open HtmlAgilityPack

open GWallet.Backend
open GWallet.Backend.FSharpUtil.UwpHacks

type IncompatibleServerException(message) =
    inherit CommunicationUnsuccessfulException(message)

type IncompatibleProtocolException(message) =
    inherit IncompatibleServerException(message)

type ServerTooNewException(message) =
    inherit IncompatibleProtocolException(message)

type ServerTooOldException(message) =
    inherit IncompatibleProtocolException(message)

type TlsNotSupportedYetInGWalletException(message) =
    inherit IncompatibleServerException(message)

type TorNotSupportedYetInGWalletException(message) =
    inherit IncompatibleServerException(message)

type ElectrumServer =
    {
        Fqdn: string;
        PrivatePort: Option<uint32>
        UnencryptedPort: Option<uint32>
    }
    member self.CheckCompatibility (): unit =
        if self.UnencryptedPort.IsNone then
            raise <| TlsNotSupportedYetInGWalletException "TLS not yet supported"
        if self.Fqdn.EndsWith ".onion" then
            raise <| TorNotSupportedYetInGWalletException "Tor(onion) not yet supported"

module ElectrumServerSeedList =

    let private FilterCompatibleServer (electrumServer: ElectrumServer) =
        try
            electrumServer.CheckCompatibility()
            true
        with
        | :? IncompatibleServerException -> false

    let ExtractServerListFromWebPage (currency: Currency): seq<ElectrumServer> =
        if not (currency.IsUtxo()) then
            failwith "This method is only compatible with UTXO currencies"

        let currencyMnemonic =
            match currency with
            | Currency.BTC -> "btc"
            | Currency.LTC -> "ltc"
            | _ -> failwith <| SPrintF1 "UTXO currency unknown to this algorithm: %A" currency

        let url = SPrintF1 "https://1209k.com/bitcoin-eye/ele.php?chain=%s" currencyMnemonic
        let web = HtmlWeb()
        let doc = web.Load url
        let firstTable = (doc.DocumentNode.SelectNodes "//table").[0]
        let tableBody = firstTable.SelectSingleNode "tbody"
        let servers = tableBody.SelectNodes "tr"
        seq {
            for i in 0..(servers.Count - 1) do
                let server = servers.[i]
                let serverProperties = server.SelectNodes "td"

                if serverProperties.Count = 0 then
                    failwith "Unexpected property count: 0"
                let fqdn = serverProperties.[0].InnerText

                if serverProperties.Count < 2 then
                    failwith <| SPrintF2 "Unexpected property count in server %s: %i" fqdn serverProperties.Count
                let port = UInt32.Parse serverProperties.[1].InnerText

                if serverProperties.Count < 3 then
                    failwith <| SPrintF3 "Unexpected property count in server %s:%i: %i" fqdn port serverProperties.Count
                let portType = serverProperties.[2].InnerText

                let encrypted =
                    match portType with
                    | "ssl" -> true
                    | "tcp" -> false
                    | _ -> failwith <| SPrintF1 "Got new unexpected port type: %s" portType
                let privatePort =
                    if encrypted then
                        Some port
                    else
                        None
                let unencryptedPort =
                    if encrypted then
                        None
                    else
                        Some port

                yield
                    {
                        Fqdn = fqdn
                        PrivatePort = privatePort
                        UnencryptedPort = unencryptedPort
                    }
        } |> Seq.filter FilterCompatibleServer

    let private ExtractServerListFromElectrumJsonFile jsonContents =
        let serversParsed = JsonValue.Parse jsonContents
        let servers =
            seq {
                for (key,value) in serversParsed.Properties do
                    let maybeUnencryptedPort = value.TryGetProperty "t"
                    let unencryptedPort =
                        match maybeUnencryptedPort with
                        | None -> None
                        | Some portAsString -> Some (UInt32.Parse (portAsString.AsString()))
                    let maybeEncryptedPort = value.TryGetProperty "s"
                    let encryptedPort =
                        match maybeEncryptedPort with
                        | None -> None
                        | Some portAsString -> Some (UInt32.Parse (portAsString.AsString()))
                    yield { Fqdn = key;
                            PrivatePort = encryptedPort;
                            UnencryptedPort = unencryptedPort;
                          }
            }
        servers |> List.ofSeq

    let ExtractServerListFromElectrumRepository (currency: Currency) =
        if not (currency.IsUtxo()) then
            failwith "This method is only compatible with UTXO currencies"

        let urlToElectrumJsonFile =
            match currency with
            | Currency.BTC -> "https://raw.githubusercontent.com/spesmilo/electrum/master/electrum/servers.json"
            | Currency.LTC -> "https://raw.githubusercontent.com/pooler/electrum-ltc/master/electrum_ltc/servers.json"
            | _ -> failwith <| SPrintF1 "UTXO currency unknown to this algorithm: %A" currency

        use webClient = new WebClient()
        let serverListInJson = webClient.DownloadString urlToElectrumJsonFile
        ExtractServerListFromElectrumJsonFile serverListInJson
            |> Seq.filter FilterCompatibleServer

    let DefaultBtcList =
        Caching.Instance.GetServers Currency.BTC
            |> List.ofSeq

    let DefaultLtcList =
        Caching.Instance.GetServers Currency.LTC
            |> List.ofSeq

    let Randomize currency =
        let serverList =
            match currency with
            | BTC -> DefaultBtcList
            | LTC -> DefaultLtcList
            | _ -> failwith <| SPrintF1 "Currency %A is not UTXO" currency
        Shuffler.Unsort serverList
