namespace GWallet.Backend.UtxoCoin.Lightning

// the only reason this Helpers file exists is because calling these methods directly would cause the frontend to need to
// reference DotNetLightning or NBitcoin directly, so: TODO: report this bug against the F# compiler
// (related: https://stackoverflow.com/questions/62274013/fs0074-the-type-referenced-through-c-crecord-is-defined-in-an-assembly-that-i)

open GWallet.Backend

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
    let public OpenChannel (lightningNode: Node) = lightningNode.OpenChannel
    let public AcceptChannel (lightningNode: Node) = lightningNode.AcceptChannel ()
    let public SendMonoHopPayment (lightningNode: Node) = lightningNode.SendMonoHopPayment
    let public ReceiveMonoHopPayment (lightningNode: Node) = lightningNode.ReceiveMonoHopPayment
    let public LockChannelFunding (lightningNode: Node) = lightningNode.LockChannelFunding
    let public EndPoint (lightningNode: Node) = lightningNode.EndPoint
