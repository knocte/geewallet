namespace GWallet.Backend

open System
open System.Reflection
open System.Text.RegularExpressions
open System.Runtime.Serialization

open Newtonsoft.Json
open Newtonsoft.Json.Serialization

open GWallet.Backend.FSharpUtil.UwpHacks

type ExceptionDetails =
    {
        ExceptionType: string
        Message: string
        StackTrace: string
        InnerException: Option<ExceptionDetails>
    }

type MarshalledException =
    {
        HumanReadableSummary: ExceptionDetails
        FullBinaryForm: string
    }
    static member private ExtractBasicDetailsFromException (ex: Exception) =
        let stackTrace =
            if ex.StackTrace = null then
                String.Empty
            else
                ex.StackTrace
        let stub =
            {
                ExceptionType = ex.GetType().FullName
                Message = ex.Message
                StackTrace = stackTrace
                InnerException = None
            }

        match ex.InnerException with
        | null -> stub
        | someNonNullInnerException ->
            let innerExceptionDetails =
                MarshalledException.ExtractBasicDetailsFromException someNonNullInnerException

            {
                stub with
                    InnerException = Some innerExceptionDetails
            }

    static member Create (ex: Exception) =
        {
            HumanReadableSummary = MarshalledException.ExtractBasicDetailsFromException ex
            FullBinaryForm = BinaryMarshalling.SerializeToString ex
        }

type DeserializationException =
    inherit Exception

    new(message: string, innerException: Exception) = { inherit Exception(message, innerException) }
    new(message: string) = { inherit Exception(message) }
    new(info: SerializationInfo, context: StreamingContext) =
        { inherit Exception(info, context) }

type SerializationException(message:string, innerException: Exception) =
    inherit Exception (message, innerException)

type MarshallingCompatibilityException =
    inherit Exception

    new(message: string, innerException: Exception) = { inherit Exception(message, innerException) }
    new(info: SerializationInfo, context: StreamingContext) =
        { inherit Exception(info, context) }

type VersionMismatchDuringDeserializationException =
    inherit DeserializationException

    new (message: string, innerException: Exception) =
        { inherit DeserializationException (message, innerException) }
    new (info: SerializationInfo, context: StreamingContext) =
        { inherit DeserializationException (info, context) }

module internal VersionHelper =
    let internal CURRENT_VERSION =
        Assembly.GetExecutingAssembly().GetName().Version.ToString()

type MarshallingWrapper<'T> =
    {
        Version: string
        TypeName: string
        Value: 'T
    }
    static member New value =
        {
            Value = value
            Version = VersionHelper.CURRENT_VERSION
            TypeName = typeof<'T>.FullName
        }

type private PascalCase2LowercasePlusUnderscoreContractResolver() =
    inherit DefaultContractResolver()

    // https://stackoverflow.com/a/20952003/544947
    let pascalToUnderScoreRegex = Regex("((?<=.)[A-Z][a-zA-Z]*)|((?<=[a-zA-Z])\d+)", RegexOptions.Multiline)
    let pascalToUnderScoreReplacementExpression = "_$1$2"
    override __.ResolvePropertyName (propertyName: string) =
        pascalToUnderScoreRegex.Replace(propertyName, pascalToUnderScoreReplacementExpression).ToLower()

// combine https://stackoverflow.com/a/48330214/544947 with https://stackoverflow.com/a/29660550/544947
// (because null values should map to None values in the case of Option<> types, otherwise tests fail)
type RequireAllPropertiesContractResolver() =
    inherit DefaultContractResolver()

    override __.CreateObjectContract(objectType: Type) =
        let contract = base.CreateObjectContract objectType
        contract.ItemRequired <- Nullable<Required> Required.Always
        contract

    override __.CreateProperty(memberInfo: MemberInfo, memberSerialization: MemberSerialization) =
        let property = base.CreateProperty(memberInfo, memberSerialization)
        // https://stackoverflow.com/questions/20696262/reflection-to-find-out-if-property-is-of-option-type
        let isOption =
            property.PropertyType.IsGenericType &&
            property.PropertyType.GetGenericTypeDefinition() = typedefof<Option<_>>
        if isOption then
            property.Required <- Required.AllowNull
        property

module Marshalling =

    let DefaultFormatting =
#if DEBUG
        Formatting.Indented
#else
        Formatting.None
#endif

    let internal PascalCase2LowercasePlusUnderscoreConversionSettings =
        JsonSerializerSettings(ContractResolver = PascalCase2LowercasePlusUnderscoreContractResolver())

    let internal DefaultSettings () = // Function so that we won't mutate. This is hard to clone.
        JsonSerializerSettings(MissingMemberHandling = MissingMemberHandling.Error,
                               ContractResolver = RequireAllPropertiesContractResolver(),
                               DateTimeZoneHandling = DateTimeZoneHandling.Utc)

    let private currentVersion = VersionHelper.CURRENT_VERSION

    let ExtractType(json: string): Type =
        let wrapper = JsonConvert.DeserializeObject<MarshallingWrapper<obj>> json
        if Object.ReferenceEquals(null, wrapper) then
            failwith <| SPrintF1 "Failed to extract type from JSON: %s" json
        Type.GetType wrapper.TypeName

    let DeserializeCustom<'T>(json: string, settings: JsonSerializerSettings): 'T =
        if (json = null) then
            raise (ArgumentNullException("json"))
        if (String.IsNullOrWhiteSpace(json)) then
            raise (ArgumentException("empty or whitespace json", "json"))

        let deserialized =
            try
                JsonConvert.DeserializeObject<MarshallingWrapper<'T>>(json, settings)
            with
            | ex ->
                let versionJsonTag = "\"Version\":\""
                if (json.Contains(versionJsonTag)) then
                    let jsonSinceVersion = json.Substring(json.IndexOf(versionJsonTag) + versionJsonTag.Length)
                    let endVersionIndex = jsonSinceVersion.IndexOf("\"")
                    let version = jsonSinceVersion.Substring(0, endVersionIndex)
                    if (version <> currentVersion) then
                        let msg = SPrintF2 "Incompatible marshalling version found (%s vs. current %s) while trying to deserialize JSON"
                                          version currentVersion
                        raise <| VersionMismatchDuringDeserializationException(msg, ex)

                let targetTypeName = typeof<'T>.FullName
                raise <| DeserializationException(SPrintF2 "Exception when trying to deserialize (to type '%s') from string '%s'" targetTypeName json, ex)


        if Object.ReferenceEquals(deserialized, null) then
            raise <| DeserializationException(SPrintF1 "JsonConvert.DeserializeObject returned null when trying to deserialize '%s'"
                                                      json)
        if Object.ReferenceEquals(deserialized.Value, null) then
            raise <| DeserializationException(SPrintF1 "JsonConvert.DeserializeObject could not deserialize the Value member of '%s'"
                                                      json)
        deserialized.Value

    let Deserialize<'T>(json: string): 'T =
        match typeof<'T> with
        | theType when typeof<Exception>.IsAssignableFrom theType ->
            let marshalledException: MarshalledException = DeserializeCustom (json, DefaultSettings())
            BinaryMarshalling.DeserializeFromString marshalledException.FullBinaryForm :?> 'T
        | _ ->
            DeserializeCustom (json, DefaultSettings())

    let private SerializeInternal<'T>(value: 'T) (settings: JsonSerializerSettings) (formatting: Formatting): string =
        JsonConvert.SerializeObject(MarshallingWrapper<'T>.New value,
                                    formatting,
                                    settings)

    let SerializeCustom<'T>(value: 'T, settings: JsonSerializerSettings, formatting: Formatting): string =
        try
            SerializeInternal value settings formatting
        with
        | exn ->
            raise (SerializationException(SPrintF2 "Could not serialize object of type '%s' and value '%A'"
                                                  (typeof<'T>.FullName) value, exn))

    let Serialize<'T>(value: 'T): string =
        match box value with
        | :? Exception as ex ->
            let exToSerialize = MarshalledException.Create ex
            let serializedEx = SerializeCustom (exToSerialize, DefaultSettings (), DefaultFormatting)

            try
                let _deserializedEx: 'T = Deserialize serializedEx
                ()
            with
            | ex ->
                raise
                <| MarshallingCompatibilityException (
                    SPrintF1
                        "Exception type '%s' could not be serialized. Maybe it lacks the required '(info: SerializationInfo, context: StreamingContext)' constructor?"
                        typeof<'T>.FullName, ex)

            serializedEx
        | _ ->
            SerializeCustom (value, DefaultSettings (), DefaultFormatting)

    let SerializeOneLine<'T>(value: 'T): string =
        SerializeCustom (value, DefaultSettings (), Formatting.None)
