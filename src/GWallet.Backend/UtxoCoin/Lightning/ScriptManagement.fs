namespace GWallet.Backend.UtxoCoin.Lightning

open NBitcoin

open GWallet.Backend
open GWallet.Backend.FSharpUtil
open GWallet.Backend.UtxoCoin
open DotNetLightning.Utils

module ScriptManager =
    // Used for e.g. option_upfront_shutdown_script in
    // https://github.com/lightningnetwork/lightning-rfc/blob/master/02-peer-protocol.md#rationale-4
    let CreatePayoutScript (account: NormalUtxoAccount) =
        let baseAccount = account :> IAccount
        let network = Account.GetNetwork baseAccount.Currency
        let scriptAddress = BitcoinScriptAddress (baseAccount.PublicAddress, network)
        UnwrapResult (ShutdownScriptPubKey.TryFromScript scriptAddress.ScriptPubKey) "should not happen"