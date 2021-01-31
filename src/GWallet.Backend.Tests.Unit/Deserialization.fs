namespace GWallet.Backend.Tests.Unit

open System

open NUnit.Framework

open GWallet.Backend

[<TestFixture>]
type Deserialization() =

    [<Test>]
    member __.``deserialize cache does not fail``() =

        let deserializedCache: DietCache = Marshalling.Deserialize
                                               MarshallingData.SofisticatedCachingDataExampleInJson

        Assert.That(deserializedCache, Is.Not.Null)

        Assert.That(deserializedCache.Addresses, Is.Not.Null)
        let etcAddresses = deserializedCache.Addresses.TryFind "0xFOOBARBAZ"
        Assert.That etcAddresses.IsSome
        let addresses = etcAddresses.Value
        Assert.That(addresses, Is.EqualTo [Currency.ETC.ToString()])

        Assert.That(deserializedCache.Balances, Is.Not.Null)
        let etcBalance = deserializedCache.Balances.TryFind (Currency.ETC.ToString())
        Assert.That(etcBalance.IsSome)
        let balance = etcBalance.Value
        Assert.That(balance, Is.EqualTo(123456789.12345678m))

        Assert.That(deserializedCache.UsdPrice, Is.Not.Null)
        let price = deserializedCache.UsdPrice.Item (Currency.ETH.ToString())
        Assert.That(price, Is.EqualTo(161.796))

    [<Test>]
    member __.``unsigned btc transaction import``() =
        let deserializedUnsignedTrans: UnsignedTransaction<IBlockchainFeeInfo> =
            Account.ImportUnsignedTransactionFromJson
                MarshallingData.UnsignedBtcTransactionExampleInJson

        Assert.That(deserializedUnsignedTrans, Is.Not.Null)
        Assert.That(deserializedUnsignedTrans.Proposal, Is.Not.Null)
        Assert.That(deserializedUnsignedTrans.Cache, Is.Not.Null)
        Assert.That(deserializedUnsignedTrans.Metadata, Is.Not.Null)

        Assert.That(deserializedUnsignedTrans.Proposal.Amount.ValueToSend, Is.EqualTo(10.01m))
        Assert.That(deserializedUnsignedTrans.Proposal.Amount.BalanceAtTheMomentOfSending,
                    Is.EqualTo 12.02m)
        Assert.That(deserializedUnsignedTrans.Proposal.Amount.Currency, Is.EqualTo Currency.BTC)
        Assert.That(deserializedUnsignedTrans.Proposal.DestinationAddress,
                    Is.EqualTo("13jxHQDxGto46QhjFiMb78dZdys9ZD8vW5"))
        Assert.That(deserializedUnsignedTrans.Proposal.OriginAddress,
                    Is.EqualTo("16pKBjGGZkUXo1afyBNf5ttFvV9hauS1kR"))

        let btcTxMetadata = deserializedUnsignedTrans.Metadata :?> UtxoCoin.TransactionMetadata
        Assert.That(btcTxMetadata.Fee.EstimatedFeeInSatoshis, Is.EqualTo 10)
        Assert.That(btcTxMetadata.Inputs.Length, Is.EqualTo 1)
        Assert.That(deserializedUnsignedTrans.Metadata.FeeEstimationTime,
                    Is.EqualTo MarshallingData.SomeDate)

        Assert.That(deserializedUnsignedTrans.Cache.Balances.Count, Is.EqualTo 5)
        Assert.That(deserializedUnsignedTrans.Cache.UsdPrice.Count, Is.EqualTo 5)

    [<Test>]
    member __.``unsigned ether transaction import``() =
        let deserializedUnsignedTrans: UnsignedTransaction<IBlockchainFeeInfo> =
            Account.ImportUnsignedTransactionFromJson
                MarshallingData.UnsignedEtherTransactionExampleInJson

        Assert.That(deserializedUnsignedTrans, Is.Not.Null)
        Assert.That(deserializedUnsignedTrans.Proposal, Is.Not.Null)
        Assert.That(deserializedUnsignedTrans.Cache, Is.Not.Null)
        Assert.That(deserializedUnsignedTrans.Metadata, Is.Not.Null)

        Assert.That(deserializedUnsignedTrans.Proposal.Amount.ValueToSend, Is.EqualTo(10.01m))
        Assert.That(deserializedUnsignedTrans.Proposal.Amount.BalanceAtTheMomentOfSending,
                    Is.EqualTo 12.02m)
        Assert.That(deserializedUnsignedTrans.Proposal.Amount.Currency, Is.EqualTo Currency.ETC)
        Assert.That(deserializedUnsignedTrans.Proposal.DestinationAddress,
                    Is.EqualTo("0xf3j4m0rjxdddud9403j"))
        Assert.That(deserializedUnsignedTrans.Proposal.OriginAddress,
                    Is.EqualTo("0xf3j4m0rjx94sushh03j"))

        let etherTxMetadata = deserializedUnsignedTrans.Metadata :?> Ether.TransactionMetadata
        Assert.That(etherTxMetadata.TransactionCount, Is.EqualTo(69))
        Assert.That(etherTxMetadata.Fee.Currency, Is.EqualTo(Currency.ETC))
        Assert.That(etherTxMetadata.Fee.GasPriceInWei, Is.EqualTo(6969))
        Assert.That(deserializedUnsignedTrans.Metadata.FeeEstimationTime,
                    Is.EqualTo MarshallingData.SomeDate)

        Assert.That(deserializedUnsignedTrans.Cache.Balances.Count, Is.EqualTo(0))
        Assert.That(deserializedUnsignedTrans.Cache.UsdPrice.Count, Is.EqualTo(0))

    [<Test>]
    member __.``signed btc transaction import``() =

        let deserializedSignedTrans: SignedTransaction<IBlockchainFeeInfo> =
            Account.ImportSignedTransactionFromJson
                MarshallingData.SignedBtcTransactionExampleInJson

        Assert.That(deserializedSignedTrans, Is.Not.Null)

        Assert.That(deserializedSignedTrans.RawTransaction,
            Is.EqualTo "0200000000010111b6e0460bb810b05744f8d38262f95fbab02b168b070598a6f31fad438fced4000000001716001427c106013c0042da165c082b3870c31fb3ab4683feffffff0200ca9a3b0000000017a914d8b6fcc85a383261df05423ddf068a8987bf0287873067a3fa0100000017a914d5df0b9ca6c0e1ba60a9ff29359d2600d9c6659d870247304402203b85cb05b43cc68df72e2e54c6cb508aa324a5de0c53f1bbfe997cbd7509774d022041e1b1823bdaddcd6581d7cde6e6a4c4dbef483e42e59e04dbacbaf537c3e3e8012103fbbdb3b3fc3abbbd983b20a557445fb041d6f21cc5977d2121971cb1ce5298978c000000")

        Assert.That(deserializedSignedTrans.TransactionInfo, Is.Not.Null)
        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal, Is.Not.Null)
        Assert.That(deserializedSignedTrans.TransactionInfo.Cache, Is.Not.Null)
        Assert.That(deserializedSignedTrans.TransactionInfo.Metadata, Is.Not.Null)

        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal.Amount.ValueToSend,
                    Is.EqualTo(10.01m))
        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal.Amount.BalanceAtTheMomentOfSending,
                    Is.EqualTo 12.02m)
        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal.Amount.Currency,
                    Is.EqualTo(Currency.BTC))
        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal.DestinationAddress,
                    Is.EqualTo("13jxHQDxGto46QhjFiMb78dZdys9ZD8vW5"))
        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal.OriginAddress,
                    Is.EqualTo("16pKBjGGZkUXo1afyBNf5ttFvV9hauS1kR"))

        let btcTxMetadata = deserializedSignedTrans.TransactionInfo.Metadata :?> UtxoCoin.TransactionMetadata
        Assert.That(btcTxMetadata.Fee.EstimatedFeeInSatoshis, Is.EqualTo 10)
        Assert.That(btcTxMetadata.Inputs.Length, Is.EqualTo 1)
        Assert.That(deserializedSignedTrans.TransactionInfo.Metadata.FeeEstimationTime,
                    Is.EqualTo MarshallingData.SomeDate)

        Assert.That(deserializedSignedTrans.TransactionInfo.Cache.Balances.Count,
                    Is.EqualTo 5)
        Assert.That(deserializedSignedTrans.TransactionInfo.Cache.UsdPrice.Count,
                    Is.EqualTo 5)

    [<Test>]
    member __.``signed ether transaction import``() =

        let deserializedSignedTrans: SignedTransaction<IBlockchainFeeInfo> =
            Account.ImportSignedTransactionFromJson
                MarshallingData.SignedEtherTransactionExampleInJson

        Assert.That(deserializedSignedTrans, Is.Not.Null)

        Assert.That(deserializedSignedTrans.RawTransaction,
            Is.EqualTo("doijfsoifjdosisdjfomirmjosmi"))

        Assert.That(deserializedSignedTrans.TransactionInfo, Is.Not.Null)
        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal, Is.Not.Null)
        Assert.That(deserializedSignedTrans.TransactionInfo.Cache, Is.Not.Null)
        Assert.That(deserializedSignedTrans.TransactionInfo.Metadata, Is.Not.Null)

        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal.Amount.ValueToSend,
                    Is.EqualTo(10.01m))
        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal.Amount.BalanceAtTheMomentOfSending,
                    Is.EqualTo 12.02m)
        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal.Amount.Currency,
                    Is.EqualTo(Currency.ETC))
        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal.DestinationAddress,
                    Is.EqualTo("0xf3j4m0rjxdddud9403j"))
        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal.OriginAddress,
                    Is.EqualTo("0xf3j4m0rjx94sushh03j"))

        let etherTxMetadata = deserializedSignedTrans.TransactionInfo.Metadata :?> Ether.TransactionMetadata
        Assert.That(etherTxMetadata.TransactionCount, Is.EqualTo(69))
        Assert.That(etherTxMetadata.Fee.Currency,
                    Is.EqualTo(Currency.ETC))
        Assert.That(etherTxMetadata.Fee.GasPriceInWei,
                    Is.EqualTo(6969))
        Assert.That(deserializedSignedTrans.TransactionInfo.Metadata.FeeEstimationTime,
                    Is.EqualTo MarshallingData.SomeDate)

        Assert.That(deserializedSignedTrans.TransactionInfo.Cache.Balances.Count,
                    Is.EqualTo(2))
        Assert.That(deserializedSignedTrans.TransactionInfo.Cache.UsdPrice.Count,
                    Is.EqualTo(2))

    [<Test>]
    member __.``unsigned SAI transaction import``() =
        let deserializedUnsignedTrans: UnsignedTransaction<IBlockchainFeeInfo> =
            Account.ImportUnsignedTransactionFromJson
                MarshallingData.UnsignedSaiTransactionExampleInJson

        Assert.That(deserializedUnsignedTrans, Is.Not.Null)
        Assert.That(deserializedUnsignedTrans.Proposal, Is.Not.Null)
        Assert.That(deserializedUnsignedTrans.Cache, Is.Not.Null)
        Assert.That(deserializedUnsignedTrans.Metadata, Is.Not.Null)

        Assert.That(deserializedUnsignedTrans.Proposal.Amount.ValueToSend, Is.EqualTo(1m))
        Assert.That(deserializedUnsignedTrans.Proposal.Amount.BalanceAtTheMomentOfSending,
                    Is.EqualTo 7.08m)
        Assert.That(deserializedUnsignedTrans.Proposal.Amount.Currency, Is.EqualTo Currency.SAI)
        Assert.That(deserializedUnsignedTrans.Proposal.DestinationAddress,
                    Is.EqualTo("0xDb0381B1a380d8db2724A9Ca2d33E0C6C044bE3b"))
        Assert.That(deserializedUnsignedTrans.Proposal.OriginAddress,
                    Is.EqualTo("0xba766d6d13E2Cc921Bf6e896319D32502af9e37E"))

        let saiTxMetadata = deserializedUnsignedTrans.Metadata :?> Ether.TransactionMetadata
        Assert.That(saiTxMetadata.TransactionCount, Is.EqualTo(7))
        Assert.That(saiTxMetadata.Fee.Currency, Is.EqualTo(Currency.ETH))
        Assert.That(saiTxMetadata.Fee.GasPriceInWei, Is.EqualTo(3343750000L))
        Assert.That(deserializedUnsignedTrans.Metadata.FeeEstimationTime,
                    Is.EqualTo MarshallingData.SomeDate)

        Assert.That(deserializedUnsignedTrans.Cache.Balances.Count, Is.EqualTo 5)
        Assert.That(deserializedUnsignedTrans.Cache.UsdPrice.Count, Is.EqualTo(5))

    [<Test>]
    member __.``signed SAI transaction import``() =

        let deserializedSignedTrans: SignedTransaction<IBlockchainFeeInfo> =
            Account.ImportSignedTransactionFromJson
                MarshallingData.SignedSaiTransactionExampleInJson

        Assert.That(deserializedSignedTrans, Is.Not.Null)

        Assert.That(deserializedSignedTrans.RawTransaction,
            Is.EqualTo("f8a80784c74d93708291b29489d24a6b4ccb1b6faa2625fe562bdd9a2326035980b844a9059cbb000000000000000000000000db0381b1a380d8db2724a9ca2d33e0c6c044be3b0000000000000000000000000000000000000000000000000de0b6b3a764000026a072cdeb03affd5977c76366efbc1405fbb4fa997ce72c1e4554ba9ec5ef772ddca069d522ea304efebd2537330870bc1ca9e9a6fe3eb5f8d8f66c1b82d9fc27a4bf"))

        Assert.That(deserializedSignedTrans.TransactionInfo, Is.Not.Null)
        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal, Is.Not.Null)
        Assert.That(deserializedSignedTrans.TransactionInfo.Cache, Is.Not.Null)
        Assert.That(deserializedSignedTrans.TransactionInfo.Metadata, Is.Not.Null)

        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal.Amount.ValueToSend,
                    Is.EqualTo(1m))
        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal.Amount.BalanceAtTheMomentOfSending,
                    Is.EqualTo 7.08m)
        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal.Amount.Currency,
                    Is.EqualTo Currency.SAI)
        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal.DestinationAddress,
                    Is.EqualTo("0xDb0381B1a380d8db2724A9Ca2d33E0C6C044bE3b"))
        Assert.That(deserializedSignedTrans.TransactionInfo.Proposal.OriginAddress,
                    Is.EqualTo("0xba766d6d13E2Cc921Bf6e896319D32502af9e37E"))

        let etherTxMetadata = deserializedSignedTrans.TransactionInfo.Metadata :?> Ether.TransactionMetadata
        Assert.That(etherTxMetadata.TransactionCount, Is.EqualTo(7))
        Assert.That(etherTxMetadata.Fee.Currency,
                    Is.EqualTo(Currency.ETH))
        Assert.That(etherTxMetadata.Fee.GasPriceInWei,
                    Is.EqualTo(3343750000L))
        Assert.That(deserializedSignedTrans.TransactionInfo.Metadata.FeeEstimationTime,
                    Is.EqualTo MarshallingData.SomeDate)

        Assert.That(deserializedSignedTrans.TransactionInfo.Cache.Balances.Count,
                    Is.EqualTo 5)
        Assert.That(deserializedSignedTrans.TransactionInfo.Cache.UsdPrice.Count,
                    Is.EqualTo(5))

    [<Test>]
    member __.``can roundtrip currency``() =
        let c = Currency.BTC
        let serialized = Marshalling.Serialize c
        let deserialized = Marshalling.Deserialize<Currency> serialized
        Assert.That(c, Is.EqualTo deserialized)
