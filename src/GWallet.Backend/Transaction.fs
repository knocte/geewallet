namespace GWallet.Backend

type UnsignedTransactionProposal =
    {
        OriginAddress: string
        Amount: TransferAmount
        DestinationAddress: string
    }

// NOTE: I wanted to mark this type below `internal`, however that breaks JSON serialization
//       in two possible ways: 1. silently (just returning {}), 2. with an exception
type UnsignedTransaction<'T when 'T :> IBlockchainFeeInfo> =
    {
        Proposal: UnsignedTransactionProposal
        Metadata: 'T
        Cache: DietCache
    }

    member self.ToAbstract () : UnsignedTransaction<IBlockchainFeeInfo> =
        {
            Metadata = self.Metadata :> IBlockchainFeeInfo
            Cache = self.Cache
            Proposal = self.Proposal
        }

type SignedTransaction<'T when 'T :> IBlockchainFeeInfo> =
    {
        TransactionInfo: UnsignedTransaction<'T>
        RawTransaction: string
    }

    member self.ToAbstract () : SignedTransaction<IBlockchainFeeInfo> =
        {
            TransactionInfo = self.TransactionInfo.ToAbstract ()
            RawTransaction = self.RawTransaction
        }
