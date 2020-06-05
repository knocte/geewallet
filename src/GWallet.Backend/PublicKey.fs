
namespace GWallet.Backend

open NBitcoin

type PublicKey(pubKey: string, currency: Currency) =
    do
        if currency.IsUtxo() then
            PubKey pubKey
            |> ignore<PubKey>

    override __.ToString() =
        pubKey

