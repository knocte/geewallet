namespace GWallet.Backend.Tests.Unit

open System.Reflection

open NUnit.Framework

open GWallet.Backend

[<TestFixture>]
type Serialization() =
    [<Test>]
    member __.``basic caching export does not fail``() =
        let json = Marshalling.Serialize MarshallingData.EmptyCachingDataExample
        Assert.That(json, Is.Not.Null)
        Assert.That(json, Is.Not.Empty)

    [<Test>]
    member __.``basic caching export is accurate``() =
        let json = Marshalling.Serialize MarshallingData.EmptyCachingDataExample
        Assert.That(json, Is.Not.Null)
        Assert.That(json, Is.Not.Empty)
        Assert.That(json, Is.EqualTo (MarshallingData.EmptyCachingDataExampleInJson))

    [<Test>]
    member __.``complex caching export works``() =

        let json = Marshalling.Serialize MarshallingData.SofisticatedCachingDataExample
        Assert.That(json, Is.Not.Null)
        Assert.That(json, Is.Not.Empty)

        Assert.That(json,
                    Is.EqualTo (MarshallingData.SofisticatedCachingDataExampleInJson))

    [<Test>]
    member __.``unsigned BTC transaction export``() =
        let json = Account.ExportUnsignedTransactionToJson
                               MarshallingData.UnsignedBtcTransactionExample
        Assert.That(json, Is.Not.Null)
        Assert.That(json, Is.Not.Empty)
        Assert.That(json |> MarshallingData.Sanitize,
                    Is.EqualTo(MarshallingData.UnsignedBtcTransactionExampleInJson))

    [<Test>]
    member __.``unsigned ether transaction export``() =
        let json = Account.ExportUnsignedTransactionToJson
                               MarshallingData.UnsignedEtherTransactionExample
        Assert.That(json, Is.Not.Null)
        Assert.That(json, Is.Not.Empty)
        Assert.That(json |> MarshallingData.Sanitize,
                    Is.EqualTo(MarshallingData.UnsignedEtherTransactionExampleInJson))

    [<Test>]
    member __.``signed btc transaction export``() =
        let json = Account.ExportUnsignedTransactionToJson MarshallingData.SignedBtcTransactionExample
        Assert.That(json, Is.Not.Null)
        Assert.That(json, Is.Not.Empty)
        Assert.That(json |> MarshallingData.Sanitize,
                    Is.EqualTo MarshallingData.SignedBtcTransactionExampleInJson)

    [<Test>]
    member __.``signed ether transaction export``() =
        let json = Account.ExportUnsignedTransactionToJson MarshallingData.SignedEtherTransactionExample
        Assert.That(json, Is.Not.Null)
        Assert.That(json, Is.Not.Empty)
        Assert.That(json |> MarshallingData.Sanitize,
                    Is.EqualTo MarshallingData.SignedEtherTransactionExampleInJson)

    [<Test>]
    member __.``unsigned SAI transaction export``() =
        let json = Account.ExportUnsignedTransactionToJson
                               MarshallingData.UnsignedSaiTransactionExample
        Assert.That(json, Is.Not.Null)
        Assert.That(json, Is.Not.Empty)
        Assert.That(json |> MarshallingData.Sanitize,
                    Is.EqualTo MarshallingData.UnsignedSaiTransactionExampleInJson)

    [<Test>]
    member __.``signed SAI transaction export``() =
        let json = Account.ExportUnsignedTransactionToJson MarshallingData.SignedSaiTransactionExample
        Assert.That(json, Is.Not.Null)
        Assert.That(json, Is.Not.Empty)
        Assert.That(json|> MarshallingData.Sanitize,
                    Is.EqualTo MarshallingData.SignedSaiTransactionExampleInJson)

    [<Test>]
    member __.``can serialize exceptions``() =
        let json = Account.ExportUnsignedTransactionToJson MarshallingData.SignedSaiTransactionExample
        Assert.That(json, Is.Not.Null)
        Assert.That(json, Is.Not.Empty)
        Assert.That(json|> MarshallingData.Sanitize,
                    Is.EqualTo MarshallingData.SignedSaiTransactionExampleInJson)
