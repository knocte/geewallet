namespace GWallet.Backend.Tests.Unit

open System
open System.Threading.Tasks

open NUnit.Framework

open GWallet.Backend

type UnexpectedTaskCanceledException(message: string, innerException) =
    inherit TaskCanceledException (message, innerException)

type TypeWithStringOverridenManually =
    | FOO
    | BAR
    override self.ToString() =
        match self with
        | FOO -> "FOO"
        | BAR -> "BAR"

type TypeWithNoToStringOverriden =
    | FOO
    | BAR

[<TestFixture>]
type FSharpUtilCoverage() =

    [<Test>]
    member __.``find exception: basic test``() =
        let innerEx = TaskCanceledException "bar"
        let wrapperEx = Exception("foo", innerEx)
        let childFound = FSharpUtil.FindException<TaskCanceledException> wrapperEx
        match childFound with
        | None -> failwith "should find through inner classes"
        | Some ex ->
            Assert.That(Object.ReferenceEquals(ex, innerEx), Is.True)
            Assert.That(Object.ReferenceEquals(ex.InnerException, null))

    [<Test>]
    member __.``find exception: it works with inherited classes (UnexpectedTaskCanceledException is child of TaskCanceledException)``() =
        let innerEx = TaskCanceledException "bar"
        let inheritedEx = UnexpectedTaskCanceledException("foo", innerEx)
        let parentFound = FSharpUtil.FindException<TaskCanceledException> inheritedEx
        match parentFound with
        | None -> failwith "should work with derived classes"
        | Some ex ->
            Assert.That(Object.ReferenceEquals(ex, inheritedEx), Is.True)
            Assert.That(Object.ReferenceEquals(ex.InnerException, innerEx))

    [<Test>]
    member __.``find exception: flattens (AggregateEx)``() =
        let innerEx1 = TaskCanceledException "bar" :> Exception
        let innerEx2 = UnexpectedTaskCanceledException ("baz", null) :> Exception
        let parent = AggregateException("foo", [|innerEx1; innerEx2|])
        let sibling1Found = FSharpUtil.FindException<TaskCanceledException> parent
        match sibling1Found with
        | None -> failwith "should work"
        | Some ex ->
            Assert.That(Object.ReferenceEquals(ex, innerEx1), Is.True)
        let sibling2Found = FSharpUtil.FindException<UnexpectedTaskCanceledException> parent
        match sibling2Found with
        | None -> failwith "should find sibling 2 too"
        | Some ex ->
            Assert.That(Object.ReferenceEquals(ex, innerEx2), Is.True)

    [<Test>]
    member __.``converts fsharp's print syntax to String-Format (basic)``() =
        let basicStr = "%s"
        Assert.That(FSharpUtil.ReflectionlessPrint.ToStringFormat basicStr, Is.EqualTo "{0}")
        Assert.That(FSharpUtil.ReflectionlessPrint.SPrintF1 basicStr "foo", Is.EqualTo "foo")

        let basicInt1 = "%i"
        Assert.That(FSharpUtil.ReflectionlessPrint.ToStringFormat basicInt1, Is.EqualTo "{0}")
        Assert.That(FSharpUtil.ReflectionlessPrint.SPrintF1 basicInt1 1, Is.EqualTo "1")

        let basicInt2 = "%d"
        Assert.That(FSharpUtil.ReflectionlessPrint.ToStringFormat basicInt2, Is.EqualTo "{0}")
        Assert.That(FSharpUtil.ReflectionlessPrint.SPrintF1 basicInt2 2, Is.EqualTo "2")

        let moreChars = "[%s]"
        Assert.That(FSharpUtil.ReflectionlessPrint.ToStringFormat moreChars, Is.EqualTo "[{0}]")
        Assert.That(FSharpUtil.ReflectionlessPrint.SPrintF1 moreChars "foo", Is.EqualTo "[foo]")

        let twoStrings = "%s-%s"
        Assert.That(FSharpUtil.ReflectionlessPrint.ToStringFormat twoStrings, Is.EqualTo "{0}-{1}")
        Assert.That(FSharpUtil.ReflectionlessPrint.SPrintF2 twoStrings "foo" "bar", Is.EqualTo "foo-bar")

        let twoElementsWithDifferentTypes = "%s-%i"
        Assert.That(FSharpUtil.ReflectionlessPrint.ToStringFormat twoElementsWithDifferentTypes, Is.EqualTo "{0}-{1}")
        Assert.That(FSharpUtil.ReflectionlessPrint.SPrintF2 twoElementsWithDifferentTypes "foo" 1,
                    Is.EqualTo "foo-1")

        let twoElementsWithDifferentTypesWithInverseOrder = "%i-%s"
        Assert.That(FSharpUtil.ReflectionlessPrint.ToStringFormat twoElementsWithDifferentTypesWithInverseOrder,
                    Is.EqualTo "{0}-{1}")
        Assert.That(FSharpUtil.ReflectionlessPrint.SPrintF2 twoElementsWithDifferentTypesWithInverseOrder 1 "foo",
                    Is.EqualTo "1-foo")

        let advancedEscaping = "%f%% done"
        Assert.That(FSharpUtil.ReflectionlessPrint.ToStringFormat advancedEscaping, Is.EqualTo "{0}% done")
        Assert.That(FSharpUtil.ReflectionlessPrint.SPrintF1 advancedEscaping 0.1, Is.EqualTo "0.1% done")

    [<Test>]
    member __.``converts fsharp's print syntax to String-Format (advanced I)``() =
        let advanced = "%A"
        Assert.That(FSharpUtil.ReflectionlessPrint.ToStringFormat advanced, Is.EqualTo "{0}")
        Assert.That(FSharpUtil.ReflectionlessPrint.SPrintF1 advanced TypeWithStringOverridenManually.FOO,
                    Is.EqualTo "FOO")

        let advanced2 = "%Ax%A"
        Assert.That(FSharpUtil.ReflectionlessPrint.ToStringFormat advanced2, Is.EqualTo "{0}x{1}")
        Assert.That(FSharpUtil.ReflectionlessPrint.SPrintF2 advanced2
                                                            TypeWithStringOverridenManually.FOO
                                                            TypeWithStringOverridenManually.BAR,
                    Is.EqualTo "FOOxBAR")

    [<Test>]
    [<Ignore "NOTE: this test fails with old F# versions (stockmono, stockoldmono CI lanes), passes with new versions (newmono lane)">]
    member __.``converts fsharp's print syntax to String-Format (advanced II)``() =
        let advanced = "%A"
        Assert.That(FSharpUtil.ReflectionlessPrint.ToStringFormat advanced, Is.EqualTo "{0}")
        Assert.That(FSharpUtil.ReflectionlessPrint.SPrintF1 advanced TypeWithNoToStringOverriden.FOO,
                    Is.EqualTo "FOO")

        let advanced2 = "%Ax%A"
        Assert.That(FSharpUtil.ReflectionlessPrint.ToStringFormat advanced2, Is.EqualTo "{0}x{1}")
        Assert.That(FSharpUtil.ReflectionlessPrint.SPrintF2 advanced2
                                                            TypeWithNoToStringOverriden.FOO
                                                            TypeWithNoToStringOverriden.BAR,
                    Is.EqualTo "FOOxBAR")

