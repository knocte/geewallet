namespace GWallet.Backend.UtxoCoin.Lightning

// the only reason this Helpers file exists is because calling these methods directly would cause the frontend to need to
// reference DotNetLightning or NBitcoin directly, so: TODO: report this bug against the F# compiler
// (this is the easiest way to reproduce: 1. checkout repo at same commit hash where the commit that adds this comment,
//  2. add `member val internal Network = UtxoCoin.Account.GetNetwork (account :> IAccount).Currency` to the type
//  ChannelStore), 3. compile: see failing errors about wanting to link NBitcoin from Frontend.Console, even for an
// internal element of the API!
// (related: https://stackoverflow.com/questions/62274013/fs0074-the-type-referenced-through-c-crecord-is-defined-in-an-assembly-that-i)

open System

open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks

module public ChannelId =
    let ToString (channelId: ChannelIdentifier): string =
        channelId.ToString()

module public TxId =
    let ToString (txId: TransactionIdentifier) =
        txId.ToString()

module public PubKey =
    let public Parse = PublicKey.Parse

// FIXME: find a better name? as it clashes with NBitcoin's Network
module public Network =
    let public OpenChannel (nodeClient: NodeClient) = nodeClient.OpenChannel
    let public CloseChannel (nodeClient: NodeClient) = nodeClient.InitiateCloseChannel

    [<Obsolete "Use ReceiveLightningEvent instead">]
    let public AcceptCloseChannel (nodeServer: NodeServer) = nodeServer.AcceptCloseChannel

    let public CheckClosingFinished (channel: ChannelInfo): Async<bool> =
        async {
            let! resCheck = ClosedChannel.CheckClosingFinished channel.FundingTxId.DnlTxId channel.Currency
            match resCheck with
            | Ok res ->
                return res
            | Error err ->
                return failwith <| SPrintF1 "Error when checking if channel finished closing: %s" (err :> IErrorMsg).Message
        }

    let public SendHtlcPayment (nodeClient: NodeClient) = nodeClient.SendHtlcPayment
    let public SendMonoHopPayment (nodeClient: NodeClient) = nodeClient.SendMonoHopPayment
    let public ConnectLockChannelFunding (nodeClient: NodeClient) = nodeClient.ConnectLockChannelFunding

    let public AcceptChannel (nodeServer: NodeServer) = nodeServer.AcceptChannel ()

    [<Obsolete "Use ReceiveLightningEvent instead">]
    let public ReceiveMonoHopPayment (nodeServer: NodeServer) = nodeServer.ReceiveMonoHopPayment

    let public ReceiveLightningEvent (nodeServer: NodeServer) = nodeServer.ReceiveLightningEvent
    let public AcceptLockChannelFunding (nodeServer: NodeServer) = nodeServer.AcceptLockChannelFunding

    let public EndPoint (nodeServer: NodeServer) = nodeServer.EndPoint

    let public AcceptUpdateFee (lightningNode: NodeServer) = lightningNode.AcceptUpdateFee
