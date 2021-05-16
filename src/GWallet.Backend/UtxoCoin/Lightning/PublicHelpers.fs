﻿namespace GWallet.Backend.UtxoCoin.Lightning

// the only reason this Helpers file exists is because calling these methods directly would cause the frontend to need to
// reference DotNetLightning or NBitcoin directly, so: TODO: report this bug against the F# compiler
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
            let! resCheck = ClosedChannel.CheckClosingFinished channel.FundingTxId.DnlTxId
            match resCheck with
            | Ok res ->
                return res
            | Error err ->
                return failwith <| SPrintF1 "Error when checking if channel finished closing: %s" (err :> IErrorMsg).Message
        }

    let public SendMonoHopPayment (nodeClient: NodeClient) = nodeClient.SendMonoHopPayment
    let public ConnectLockChannelFunding (nodeClient: NodeClient) = nodeClient.ConnectLockChannelFunding

    let public AcceptChannel (nodeServer: NodeServer) = nodeServer.AcceptChannel ()

    [<Obsolete "Use ReceiveLightningEvent instead">]
    let public ReceiveMonoHopPayment (nodeServer: NodeServer) = nodeServer.ReceiveMonoHopPayment

    let public ReceiveLightningEvent (nodeServer: NodeServer) = nodeServer.ReceiveLightningEvent
    let public AcceptLockChannelFunding (nodeServer: NodeServer) = nodeServer.AcceptLockChannelFunding

    let public CheckForChannelFraudAndSendRevocationTx (_node: Node) =
        raise <| NotImplementedException ()

    let public CreateRecoveryTxForRemoteForceClose (node: Node) =
        node.CreateRecoveryTxForRemoteForceClose

    let public EndPoint (nodeServer: NodeServer) = nodeServer.EndPoint
