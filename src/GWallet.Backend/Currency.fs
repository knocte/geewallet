namespace GWallet.Backend

open System.Linq
open System.ComponentModel

open GWallet.Backend.FSharpUtil.UwpHacks

// this attribute below is for Json.NET (Newtonsoft.Json) to be able to deserialize this as a dict key
[<TypeConverter(typeof<StringTypeConverter>)>]
type Currency =
    // <NOTE if adding a new cryptocurrency below, remember to add it too to GetAll() and ToString()
    | BTC
    | LTC
    | ETH
    | ETC
    | DAI
    | SAI
    // </NOTE>

#if STRICTER_COMPILATION_BUT_WITH_REFLECTION_AT_RUNTIME
    static member ToStrings() =
        Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<Currency>)
            |> Array.map (fun info -> info.Name)
#endif

    static member GetAll(): seq<Currency> =
#if STRICTER_COMPILATION_BUT_WITH_REFLECTION_AT_RUNTIME
        FSharpUtil.GetAllElementsFromDiscriminatedUnion<Currency>()
#else
        seq {
            yield BTC
            yield LTC
            yield ETH
            yield ETC
            yield DAI
            yield SAI
        }
#endif

    static member Parse(currencyString: string): Currency =
        Currency.GetAll().First(fun currency -> currencyString = currency.ToString())

    member self.IsEther() =
        self = Currency.ETC || self = Currency.ETH
    member self.IsEthToken() =
        self = Currency.DAI || self = Currency.SAI
    member self.IsEtherBased() =
        self.IsEther() || self.IsEthToken()
    member self.IsUtxo() =
        self = Currency.BTC || self = Currency.LTC

    member self.DecimalPlaces(): int =
        if self.IsUtxo() then
            8
        elif self.IsEther() then
            18
        elif self = Currency.SAI || self = Currency.DAI then
            18
        else
            failwith <| SPrintF1 "Unable to determine decimal places for %A" self

    member self.BlockTimeInMinutes(): uint32 =
        match self with
        | BTC -> 10u
        | LTC -> 2u
        | _ -> failwith "Not implemented"

    override self.ToString() =
#if STRICTER_COMPILATION_BUT_WITH_REFLECTION_AT_RUNTIME
        SPrintF1 "%A" self
#else
        // when we can depend on newer versions of F#, we might be able to get rid of this (or ToString() altogther) below
        // (see FSharpUtil's test names "converts fsharp's print syntax to String-Format (advanced II)" for more info):
        match self with
        | BTC -> "BTC"
        | LTC -> "LTC"
        | ETH -> "ETH"
        | ETC -> "ETC"
        | SAI -> "SAI"
        | DAI -> "DAI"
#endif

// the reason we have used "and" is because of the circular reference
// between StringTypeConverter and Currency
and private StringTypeConverter() =
    inherit TypeConverter()
    override __.CanConvertFrom(context, sourceType) =
        sourceType = typeof<string> || base.CanConvertFrom(context, sourceType)
    override __.ConvertFrom(context, culture, value) =
        match value with
        | :? string as stringValue ->
            Seq.find (fun cur -> cur.ToString() = stringValue) (Currency.GetAll()) :> obj
        | _ -> base.ConvertFrom(context, culture, value)
