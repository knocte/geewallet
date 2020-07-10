namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Net

open GWallet.Backend
open GWallet.Backend.FSharpUtil.UwpHacks


type NodeEndPoint =
    internal {
        NodeId: PublicKey
        IPEndPoint: IPEndPoint
    }
    static member Parse (currency: Currency) (text: string): NodeEndPoint =
        let atIndex = text.IndexOf "@"
        if atIndex = -1 then
            raise <| FormatException "No '@' in endpoint string"
        let nodeIdText = text.[..atIndex - 1]
        let ipEndPointText = text.[atIndex + 1 ..]

        let portSeparatorIndex = ipEndPointText.LastIndexOf ':'
        if portSeparatorIndex = -1 then
            raise <| FormatException "No ':' after '@' in endpoint string"
        let ipAddressText = ipEndPointText.[..portSeparatorIndex - 1]
        let portText = ipEndPointText.[portSeparatorIndex + 1 ..]

        let nodeId = PublicKey.Parse currency nodeIdText
        let ipAddress = IPAddress.Parse ipAddressText
        let port = UInt16.Parse portText
        {
            NodeId = nodeId
            IPEndPoint = IPEndPoint(ipAddress, int port)
        }

    static member FromParts (nodeId: PublicKey) (ipEndPoint: IPEndPoint) =
        {
            NodeId = nodeId
            IPEndPoint = ipEndPoint
        }

    override self.ToString() =
        SPrintF2 "%s@%s" (self.NodeId.ToString()) (self.IPEndPoint.ToString())
