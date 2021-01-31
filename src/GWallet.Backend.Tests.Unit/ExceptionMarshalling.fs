namespace GWallet.Backend.Tests.Unit

open System
open System.Runtime.Serialization

open NUnit.Framework

open GWallet.Backend


type CustomExceptionWithoutSerializationCtor =
   inherit Exception

   new(message) =
       { inherit Exception(message) }

type CustomException =
   inherit Exception

   new(info: SerializationInfo, context: StreamingContext) =
       { inherit Exception(info, context) }
   new(message: string, innerException: CustomException) =
       { inherit Exception(message, innerException) }
   new(message) =
       { inherit Exception(message) }

exception CustomFSharpException


[<TestFixture>]
type ExceptionMarshalling () =

    let SerializeBasicException () =
        let ex = Exception "msg"
        Marshalling.Serialize ex

    let SerializeRealException () =
        let someEx = Exception "msg"
        let ex =
            try
                raise someEx
                someEx
            with
            | ex ->
                ex
        Marshalling.Serialize ex

    let SerializeInnerException () =
        let ex = Exception("msg", Exception "innerMsg")
        Marshalling.Serialize ex

    let SerializeCustomException () =
        let ex = CustomException "msg"
        Marshalling.Serialize ex

    let SerializeCustomFSharpException () =
        let ex = CustomFSharpException
        Marshalling.Serialize ex

    let SerializeFullException () =
        let someCEx = CustomException("msg", CustomException "innerMsg")
        let ex =
            try
                raise someCEx
                someCEx
            with
            | :? CustomException as cex ->
                cex
        Marshalling.Serialize ex


    [<Test>]
    member __.``can serialize basic exceptions``() =
        let json = SerializeBasicException ()
        Assert.That(json, Is.Not.Null)
        Assert.That(json, Is.Not.Empty)
        Assert.That(MarshallingData.SerializedExceptionsAreSame json MarshallingData.BasicExceptionExampleInJson)

    [<Test>]
    member __.``can deserialize basic exceptions``() =
        let basicExSerialized =
            try
                SerializeBasicException ()
            with
            | _ ->
                Assert.Inconclusive "Fix the serialization test first"
                failwith "unreachable"

        let ex: Exception = Marshalling.Deserialize basicExSerialized
        Assert.That(ex, Is.Not.Null)
        Assert.That(ex, Is.InstanceOf<Exception>())
        Assert.That(ex.Message, Is.EqualTo "msg")
        Assert.That(ex.InnerException, Is.Null)
        Assert.That(ex.StackTrace, Is.Null)

    [<Test>]
    member __.``can serialize real exceptions``() =
        let json = SerializeRealException ()
        Assert.That(json, Is.Not.Null)
        Assert.That(json, Is.Not.Empty)
        Assert.That(MarshallingData.SerializedExceptionsAreSame json MarshallingData.RealExceptionExampleInJson)

    [<Test>]
    member __.``can deserialize real exceptions``() =
        let realExceptionSerialized =
            try
                SerializeRealException ()
            with
            | _ ->
                Assert.Inconclusive "Fix the serialization test first"
                failwith "unreachable"

        let ex: Exception = Marshalling.Deserialize realExceptionSerialized
        Assert.That(ex, Is.Not.Null)
        Assert.That(ex, Is.InstanceOf<Exception>())
        Assert.That(ex.Message, Is.EqualTo "msg")
        Assert.That(ex.InnerException, Is.Null)
        Assert.That(ex.StackTrace, Is.Not.Null)
        Assert.That(ex.StackTrace, Is.Not.Empty)

    [<Test>]
    member __.``can serialize inner exceptions``() =
        let json = SerializeInnerException ()
        Assert.That(json, Is.Not.Null)
        Assert.That(json, Is.Not.Empty)
        Assert.That(MarshallingData.SerializedExceptionsAreSame json MarshallingData.InnerExceptionExampleInJson)

    [<Test>]
    member __.``can deserialize inner exceptions``() =
        let innerExceptionSerialized =
            try
                SerializeInnerException ()
            with
            | _ ->
                Assert.Inconclusive "Fix the serialization test first"
                failwith "unreachable"

        let ex: Exception = Marshalling.Deserialize innerExceptionSerialized
        Assert.That (ex, Is.Not.Null)
        Assert.That (ex, Is.InstanceOf<Exception>())
        Assert.That (ex.Message, Is.EqualTo "msg")
        Assert.That (ex.StackTrace, Is.Null)
        Assert.That (ex.InnerException, Is.Not.Null)

        Assert.That (ex.InnerException, Is.InstanceOf<Exception>())
        Assert.That (ex.InnerException.Message, Is.EqualTo "innerMsg")
        Assert.That (ex.InnerException.StackTrace, Is.Null)

    [<Test>]
    member __.``can serialize custom exceptions``() =
        let json = SerializeCustomException ()
        Assert.That(json, Is.Not.Null)
        Assert.That(json, Is.Not.Empty)
        Assert.That(MarshallingData.SerializedExceptionsAreSame json MarshallingData.CustomExceptionExampleInJson)

    [<Test>]
    member __.``serializing custom exception not prepared for binary serialization, throws``() =
        let exToSerialize = CustomExceptionWithoutSerializationCtor "msg"
        let ex: MarshallingCompatibilityException =
            Assert.Throws(fun _ -> Marshalling.Serialize exToSerialize |> ignore<string>)
        Assert.That(ex, Is.TypeOf<MarshallingCompatibilityException>())
        Assert.That(ex.Message, Is.StringContaining "GWallet.Backend.Tests.Unit.CustomExceptionWithoutSerializationCtor")

    [<Test>]
    member __.``can deserialize custom exceptions``() =
        let customExceptionSerialized =
            try
                SerializeCustomException ()
            with
            | _ ->
                Assert.Inconclusive "Fix the serialization test first"
                failwith "unreachable"

        let ex: Exception = Marshalling.Deserialize customExceptionSerialized
        Assert.That(ex, Is.Not.Null)
        Assert.That(ex, Is.InstanceOf<CustomException>())
        Assert.That(ex.Message, Is.EqualTo "msg")
        Assert.That(ex.InnerException, Is.Null)
        Assert.That(ex.StackTrace, Is.Null)

    [<Test>]
    member __.``can serialize F# custom exceptions``() =
        let json = SerializeCustomFSharpException ()
        Assert.That(json, Is.Not.Null)
        Assert.That(json, Is.Not.Empty)
        Assert.That(MarshallingData.SerializedExceptionsAreSame json MarshallingData.CustomFSharpExceptionExampleInJson)

    [<Test>]
    member __.``can deserialize F# custom exceptions``() =
        let customExceptionSerialized =
            try
                SerializeCustomFSharpException ()
            with
            | _ ->
                Assert.Inconclusive "Fix the serialization test first"
                failwith "unreachable"

        let ex: Exception = Marshalling.Deserialize customExceptionSerialized
        Assert.That(ex, Is.Not.Null)
        Assert.That(ex, Is.InstanceOf<CustomFSharpException>())
        Assert.That(ex.Message, Is.Not.Null)
        Assert.That(ex.Message, Is.Not.Empty)
        Assert.That(ex.InnerException, Is.Null)
        Assert.That(ex.StackTrace, Is.Null)

    // TODO: test marshalling custom exceptions with custom properties/fields, and custom F# exception with subtypes

    [<Test>]
    member __.``can serialize full exceptions (all previous features combined)``() =
        let json = SerializeFullException ()

        Assert.That(json, Is.Not.Null)
        Assert.That(json, Is.Not.Empty)

        Assert.That(MarshallingData.SerializedExceptionsAreSame json MarshallingData.FullExceptionExampleInJson)

    [<Test>]
    member __.``can deserialize full exceptions (all previous features combined)``() =
        let fullExceptionSerialized =
            try
                SerializeFullException ()
            with
            | _ ->
                Assert.Inconclusive "Fix the serialization test first"
                failwith "unreachable"

        let ex: Exception = Marshalling.Deserialize fullExceptionSerialized
        Assert.That(ex, Is.Not.Null)
        Assert.That(ex, Is.InstanceOf<CustomException> ())
        Assert.That(ex.Message, Is.Not.Null)
        Assert.That(ex.Message, Is.Not.Empty)
        Assert.That(ex.InnerException, Is.Not.Null)
        Assert.That(ex.StackTrace, Is.Not.Null)
        Assert.That(ex.StackTrace, Is.Not.Empty)

