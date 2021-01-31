namespace GWallet.Backend.Tests.Unit

open NUnit.Framework

open GWallet.Backend

[<TestFixture>]
type Formatting() =

    [<Test>]
    member __.``basic fiat thousand separator test``() =
        let someUsdDecimalAmount = 1000.12m
        let formattedAmount = Formatting.DecimalAmountRounding CurrencyType.Fiat someUsdDecimalAmount
        Assert.That(formattedAmount, Is.EqualTo "1,000.12")

    [<Test>]
    member __.``basic crypto thousand separator test``() =
        let someCryptoDecimalAmount = 1000.12m
        let formattedAmount = Formatting.DecimalAmountRounding CurrencyType.Crypto someCryptoDecimalAmount
        Assert.That(formattedAmount, Is.EqualTo "1,000.12")

    [<Test>]
    member __.``basic fiat rounding down test``() =
        let someUsdDecimalAmount = 0.013m
        let formattedAmount = Formatting.DecimalAmountRounding CurrencyType.Fiat someUsdDecimalAmount
        Assert.That(formattedAmount, Is.EqualTo "0.01")

    [<Test>]
    member __.``basic fiat rounding up test``() =
        let someUsdDecimalAmount = 0.016m
        let formattedAmount = Formatting.DecimalAmountRounding CurrencyType.Fiat someUsdDecimalAmount
        Assert.That(formattedAmount, Is.EqualTo "0.02")

    [<Test>]
    member __.``basic crypto rounding down test``() =
        let someCryptoDecimalAmount = 0.000012m
        let formattedAmount = Formatting.DecimalAmountRounding CurrencyType.Crypto someCryptoDecimalAmount
        Assert.That(formattedAmount, Is.EqualTo "0.00001")

    [<Test>]
    member __.``basic crypto rounding up test``() =
        let someCryptoDecimalAmount = 0.000016m
        let formattedAmount = Formatting.DecimalAmountRounding CurrencyType.Crypto someCryptoDecimalAmount
        Assert.That(formattedAmount, Is.EqualTo "0.00002")

    [<Test>]
    member __.``if it's not zero, even if super tiny, it shouldn't round to zero!``() =
        let someVerySmallUsdDecimalAmount = 0.0000001m
        let formattedAmount = Formatting.DecimalAmountRounding CurrencyType.Fiat someVerySmallUsdDecimalAmount
        Assert.That(formattedAmount, Is.EqualTo "0.01")

        let someVerySmallBtcDecimalAmount = 0.00000001m
        let formattedAmount = Formatting.DecimalAmountRounding CurrencyType.Crypto someVerySmallBtcDecimalAmount
        Assert.That(formattedAmount, Is.EqualTo "0.00001")

    [<Test>]
    member __.``trailing zeros always with fiat``() =
        let someUsdDecimalAmount1 = 2m
        let formattedAmount1 = Formatting.DecimalAmountRounding CurrencyType.Fiat someUsdDecimalAmount1
        Assert.That(formattedAmount1, Is.EqualTo "2.00")

        let someUsdDecimalAmount2 = 2.1m
        let formattedAmount2 = Formatting.DecimalAmountRounding CurrencyType.Fiat someUsdDecimalAmount2
        Assert.That(formattedAmount2, Is.EqualTo "2.10")

    [<Test>]
    member __.``no trailing zeros with crypto``() =
        let someCryptoDecimalAmount1 = 2m
        let formattedAmount1 = Formatting.DecimalAmountRounding CurrencyType.Crypto someCryptoDecimalAmount1
        Assert.That(formattedAmount1, Is.EqualTo "2")

        let someCryptoDecimalAmount2 = 2.1m
        let formattedAmount2 = Formatting.DecimalAmountRounding CurrencyType.Crypto someCryptoDecimalAmount2
        Assert.That(formattedAmount2, Is.EqualTo "2.1")

    [<Test>]
    member __.``basic fiat truncating exact amount test``() =
        let someUsdDecimalAmount = 1000.55m
        let maxAmount = someUsdDecimalAmount
        let formattedAmount = Formatting.DecimalAmountTruncating CurrencyType.Fiat someUsdDecimalAmount maxAmount
        Assert.That(formattedAmount, Is.EqualTo "1,000.55")

    [<Test>]
    member __.``basic crypto truncating exact amount test``() =
        let someCryptoDecimalAmount = 12.56m
        let maxAmount = someCryptoDecimalAmount
        let formattedAmount = Formatting.DecimalAmountTruncating CurrencyType.Crypto someCryptoDecimalAmount maxAmount
        Assert.That(formattedAmount, Is.EqualTo "12.56")

    [<Test>]
    member __.``basic fiat truncating down test``() =
        let someUsdDecimalAmount = 1000.55001m
        let maxAmount = 1000.55m
        let formattedAmount = Formatting.DecimalAmountTruncating CurrencyType.Fiat someUsdDecimalAmount maxAmount
        Assert.That(formattedAmount, Is.EqualTo "1,000.55")

    [<Test>]
    member __.``fiat truncating down test when round would surpass max``() =
        let someUsdDecimalAmount = 1000.556m
        let maxAmount = 1000.55m
        let formattedAmount = Formatting.DecimalAmountTruncating CurrencyType.Fiat someUsdDecimalAmount maxAmount
        Assert.That(formattedAmount, Is.EqualTo "1,000.55")

    [<Test>]
    member __.``basic crypto truncating down test``() =
        let someCryptoDecimalAmount = 0.200001m
        let maxAmount = 0.20000m
        let formattedAmount = Formatting.DecimalAmountTruncating CurrencyType.Crypto someCryptoDecimalAmount maxAmount
        Assert.That(formattedAmount, Is.EqualTo "0.2")

    [<Test>]
    member __.``crypto truncating down test when round would surpass max``() =
        let someCryptoDecimalAmount = 0.200006m
        let maxAmount = 0.20000m
        let formattedAmount = Formatting.DecimalAmountTruncating CurrencyType.Crypto someCryptoDecimalAmount maxAmount
        Assert.That(formattedAmount, Is.EqualTo "0.2")

    [<Test>]
    //https://gitlab.com/knocte/geewallet/issues/97
    member __.``wrong fiat truncating test``() =
        let someUsdDecimalAmount = 0.01m
        let maxAmount = 0.1m
        let formattedAmount = Formatting.DecimalAmountTruncating CurrencyType.Fiat someUsdDecimalAmount maxAmount
        Assert.That(formattedAmount, Is.EqualTo "0.01")

