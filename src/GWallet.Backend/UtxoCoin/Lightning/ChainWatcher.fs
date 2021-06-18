namespace GWallet.Backend.UtxoCoin.Lightning

open System.Linq

open NBitcoin
open DotNetLightning.Channel
open DotNetLightning.Utils
open DotNetLightning.Crypto
open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks


module public ChainWatcher =

    let internal CheckForChannelFraudAndSendRevocationTx (channelId: ChannelIdentifier)
                                                         (channelStore: ChannelStore)
                                                             : Async<Option<string>> = async {
        let serializedChannel = channelStore.LoadChannel channelId
        let currency = (channelStore.Account :> IAccount).Currency
        let fundingScriptCoin = serializedChannel.FundingScriptCoin()
        let fundingDestination: TxDestination = fundingScriptCoin.ScriptPubKey.GetDestination()
        let network = UtxoCoin.Account.GetNetwork currency
        let fundingAddress: BitcoinAddress = fundingDestination.GetAddress network
        let fundingAddressString: string = fundingAddress.ToString()

        let scriptHash =
            Account.GetElectrumScriptHashFromPublicAddress
                currency
                fundingAddressString

        let! historyList =
            Server.Query currency
                         (QuerySettings.Default ServerSelectionMode.Fast)
                         (ElectrumClient.GetBlockchainScriptHashHistory scriptHash)
                         None

        let checkIfRevokedCommitment (spendingTxInfo: BlockchainScriptHashHistoryInnerResult) : Async<Option<string>> =
            async {
                let spendingTxId = spendingTxInfo.TxHash
        
                let! spendingTxString =
                    Server.Query
                        currency
                        (QuerySettings.Default ServerSelectionMode.Fast)
                        (ElectrumClient.GetBlockchainTransaction spendingTxId)
                        None
        
                let spendingTx =
                    Transaction.Parse(spendingTxString, network)
        
        
                let obscuredCommitmentNumberOpt =
                    ForceCloseFundsRecovery.tryGetObscuredCommitmentNumber fundingScriptCoin.Outpoint spendingTx
        
                match obscuredCommitmentNumberOpt with
                | Ok obscuredCommitmentNumber ->
                    let localChannelPubKeys = serializedChannel.LocalChannelPubKeys
                    let remoteChannelPubKeys = serializedChannel.SavedChannelState.StaticChannelConfig.RemoteChannelPubKeys
        
                    let commitmentNumber =
                        obscuredCommitmentNumber.Unobscure
                            serializedChannel.SavedChannelState.StaticChannelConfig.IsFunder
                            localChannelPubKeys.PaymentBasepoint
                            remoteChannelPubKeys.PaymentBasepoint
        
                    //TODO: or we could just search based on CommitmentTxHash
                    let breachDataOpt =
                        (BreachDataStore channelStore.Account)
                            .LoadBreachData(channelId)
                            .GetBreachData(commitmentNumber)
        
                    match breachDataOpt with
                    | None -> return None
                    | Some breachData ->
                        let! txId = UtxoCoin.Account.BroadcastRawTransaction currency breachData.PenaltyTx
                        return Some <| txId
                | Error _ -> return None
            }
        
        
        return! ListAsyncTryPick historyList checkIfRevokedCommitment
    }

    let CheckForChannelFraudsAndSendRevocationTx (accounts: seq<UtxoCoin.NormalUtxoAccount>)
                                                     : seq<Async<Option<string>>> =
        seq {
            for account in accounts do
                let channelStore = ChannelStore account
                let channelIds = channelStore.ListChannelIds()

                for channelId in channelIds do
                    yield
                        CheckForChannelFraudAndSendRevocationTx channelId channelStore
        }
