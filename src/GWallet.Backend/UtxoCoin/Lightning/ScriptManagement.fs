namespace GWallet.Backend.UtxoCoin.Lightning

open NBitcoin
open DotNetLightning.Utils

open GWallet.Backend
open GWallet.Backend.UtxoCoin

module ScriptManager =

    let internal PositionInBlockFromScriptCoin currency (txId: ChannelId) (fundingSCoin: ScriptCoin)
                                                   : Async<TxIndexInBlock * BlockHeight> =
        async {
            let txIdHex: string = txId.Value.ToString ()
            let fundingDestination: TxDestination = fundingSCoin.ScriptPubKey.GetDestination ()
            let network = Account.GetNetwork currency
            let fundingAddress: BitcoinAddress = fundingDestination.GetAddress network
            let fundingAddressString: string = fundingAddress.ToString ()
            let scriptHash = Account.GetElectrumScriptHashFromPublicAddress currency fundingAddressString
            let! historyList =
                Server.Query
                    currency
                    (QuerySettings.Default ServerSelectionMode.Fast)
                    (ElectrumClient.GetBlockchainScriptHashHistory scriptHash)
                    None
            let history = Seq.head historyList
            let fundingBlockHeight = BlockHeight history.Height
            let! merkleResult =
                Server.Query
                    currency
                    (QuerySettings.Default ServerSelectionMode.Fast)
                    (ElectrumClient.GetBlockchainScriptHashMerkle txIdHex history.Height)
                    None
            return TxIndexInBlock merkleResult.Pos, fundingBlockHeight
        }

    // Used for e.g. option_upfront_shutdown_script in
    // https://github.com/lightningnetwork/lightning-rfc/blob/master/02-peer-protocol.md#rationale-4
    let internal CreatePayoutScript (account: IAccount) =
        let network = Account.GetNetwork account.Currency
        let scriptAddress = BitcoinScriptAddress (account.PublicAddress, network)
        scriptAddress.ScriptPubKey
