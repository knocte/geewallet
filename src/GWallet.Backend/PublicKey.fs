
namespace GWallet.Backend

open NBitcoin

type PublicKey private (currencyOpt: Option<Currency>, pubKey: string) =
    do
        match currencyOpt with
        | Some currency when currency.IsUtxo() ->
            // just for validation
            PubKey pubKey |> ignore
        | _ ->
            // TODO: validate ether keys too
            ()

    internal new (pubKey: PubKey) =
        PublicKey (None, pubKey.ToString ())

    override __.ToString() =
        pubKey

    static member internal Parse (currency: Currency) (pubKey: string): PublicKey =
        PublicKey (Some currency, pubKey)

