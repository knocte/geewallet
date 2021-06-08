namespace GWallet.Backend.UtxoCoin.Lightning

type MutualCloseTx =
    {
        Tx: UtxoTransaction
    }

type ForceCloseTx =
    {
        Tx: UtxoTransaction
    }

type ClosingTx =
    | MutualClose of MutualCloseTx
    | ForceClose of ForceCloseTx
