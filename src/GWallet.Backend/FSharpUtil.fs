namespace GWallet.Backend

open System
open System.Linq
open System.Threading.Tasks
open System.Runtime.ExceptionServices

open ResultUtils.Portability

#if STRICTER_COMPILATION_BUT_WITH_REFLECTION_AT_RUNTIME
open Microsoft.FSharp.Reflection
#endif

// FIXME: replace all usages of the below with native FSharp.Core's Result type (https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/results)
// when the stockmono_* lanes can use at least F# v4.5
type Either<'Val, 'Err when 'Err :> Exception> =
    | FailureResult of 'Err
    | SuccessfulValue of 'Val

module FSharpUtil =

    module ReflectionlessPrint =
        // TODO: support "%.2f" for digits precision, "%0i", and other special things: https://fsharpforfunandprofit.com/posts/printf/
        let ToStringFormat (fmt: string) =
            let rec inner (innerFmt: string) (count: uint32) =
                // TODO: support %e, %E, %g, %G, %o, %O, %x, %X, etc. see link above
                let supportedFormats = [| "%s"; "%d"; "%i"; "%u"; "%M"; "%f"; "%b" ; "%A"; |]
                let formatsFound = supportedFormats.Where(fun format -> innerFmt.IndexOf(format) >= 0)
                if formatsFound.Any() then
                    let firstIndexWhereFormatFound = formatsFound.Min(fun format -> innerFmt.IndexOf(format))
                    let firstFormat =
                        formatsFound.First(fun format -> innerFmt.IndexOf(format) = firstIndexWhereFormatFound)
                    let subEnd = innerFmt.IndexOf(firstFormat) + "%x".Length
                    let sub = innerFmt.Substring(0, subEnd)
                    let x = sub.Replace(firstFormat, "{" + count.ToString() + "}") + innerFmt.Substring(subEnd)
                    inner x (count+1u)
                else
                    innerFmt
            (inner fmt 0u).Replace("%%", "%")

        let SPrintF1 (fmt: string) (a: Object) =
            String.Format(ToStringFormat fmt, a)

        let SPrintF2 (fmt: string) (a: Object) (b: Object) =
            String.Format(ToStringFormat fmt, a, b)

        let SPrintF3 (fmt: string) (a: Object) (b: Object) (c: Object) =
            String.Format(ToStringFormat fmt, a, b, c)

        let SPrintF4 (fmt: string) (a: Object) (b: Object) (c: Object) (d: Object) =
            String.Format(ToStringFormat fmt, a, b, c, d)

        let SPrintF5 (fmt: string) (a: Object) (b: Object) (c: Object) (d: Object) (e: Object) =
            String.Format(ToStringFormat fmt, a, b, c, d, e)


    module UwpHacks =
#if STRICTER_COMPILATION_BUT_WITH_REFLECTION_AT_RUNTIME
        let SPrintF1 fmt a = sprintf fmt a

        let SPrintF2 fmt a b = sprintf fmt a b

        let SPrintF3 fmt a b c = sprintf fmt a b c

        let SPrintF4 fmt a b c d = sprintf fmt a b c d

        let SPrintF5 fmt a b c d e = sprintf fmt a b c d e
#else
        let SPrintF1 (fmt: string) (a: Object) =
            ReflectionlessPrint.SPrintF1 fmt a

        let SPrintF2 (fmt: string) (a: Object) (b: Object) =
            ReflectionlessPrint.SPrintF2 fmt a b

        let SPrintF3 (fmt: string) (a: Object) (b: Object) (c: Object) =
            ReflectionlessPrint.SPrintF3 fmt a b c

        let SPrintF4 (fmt: string) (a: Object) (b: Object) (c: Object) (d: Object) =
            ReflectionlessPrint.SPrintF4 fmt a b c d

        let SPrintF5 (fmt: string) (a: Object) (b: Object) (c: Object) (d: Object) (e: Object) =
            ReflectionlessPrint.SPrintF5 fmt a b c d e
#endif

    type internal ResultWrapper<'T>(value : 'T) =

        // hack?
        inherit Exception()

        member __.Value = value


    type IErrorMsg =
        abstract member Message: string

    let UnwrapResult<'TRes, 'TError> (result: Result<'TRes, 'TError>)
                                     (msg: string)
                                         : 'TRes =
        match result with
        | Ok value -> value
        | Error err ->
            let errorMsg =
                match box err with
                | :? IErrorMsg as e -> e.Message
                | _ -> err.ToString()
            failwith <| UwpHacks.SPrintF2 "error unwrapping Result: %s: %s" msg errorMsg

    let UnwrapOption<'T> (opt: Option<'T>)
                         (msg: string)
                             : 'T =
        match opt with
        | Some value -> value
        | None ->
            failwith <| UwpHacks.SPrintF1 "error unwrapping Option: %s" msg

    module AsyncExtensions =

        let MixedParallel2 (a: Async<'T1>) (b: Async<'T2>): Async<'T1*'T2> =
            async {
                let aJob = Async.StartChild a
                let bJob = Async.StartChild b

                let! aStartedJob = aJob
                let! bStartedJob = bJob

                let! aJobResult = aStartedJob
                let! bJobResult = bStartedJob

                return aJobResult,bJobResult
            }

        let MixedParallel3 (a: Async<'T1>) (b: Async<'T2>) (c: Async<'T3>): Async<'T1*'T2*'T3> =
            async {
                let aJob = Async.StartChild a
                let bJob = Async.StartChild b
                let cJob = Async.StartChild c

                let! aStartedJob = aJob
                let! bStartedJob = bJob
                let! cStartedJob = cJob

                let! aJobResult = aStartedJob
                let! bJobResult = bStartedJob
                let! cJobResult = cStartedJob

                return aJobResult,bJobResult,cJobResult
            }

        // efficient raise
        let private RaiseResult (e: ResultWrapper<'T>) =
            Async.FromContinuations(fun (_, econt, _) -> econt e)

        // like Async.Choice, but with no need for Option<T> types
        let WhenAny<'T>(jobs: seq<Async<'T>>): Async<'T> =
            let wrap (job: Async<'T>): Async<Option<'T>> =
                async {
                    let! res = job
                    return Some res
                }

            async {
                let wrappedJobs = jobs |> Seq.map wrap
                let! combinedRes = Async.Choice wrappedJobs
                match combinedRes with
                | Some x ->
                    return x
                | None ->
                    return failwith "unreachable"
            }

        // a mix between Async.WhenAny and Async.Choice
        let WhenAnyAndAll<'T>(jobs: seq<Async<'T>>): Async<Async<array<'T>>> =
            let taskSource = TaskCompletionSource<unit>()
            let wrap (job: Async<'T>) =
                async {
                    let! res = job
                    taskSource.TrySetResult()
                    |> ignore<bool>
                    return res
                }
            async {
                let allJobsInParallel =
                    jobs
                        |> Seq.map wrap
                        |> Async.Parallel
                        |> Async.StartChild
                let! allJobsStarted = allJobsInParallel
                let! _ = Async.AwaitTask taskSource.Task
                return allJobsStarted
            }

    let rec private ListIntersectInternal list1 list2 offset acc currentIndex =
        match list1,list2 with
        | [],[] -> List.rev acc
        | [],_ -> List.append (List.rev acc) list2
        | _,[] -> List.append (List.rev acc) list1
        | head1::tail1,head2::tail2 ->
            if currentIndex % (int offset) = 0 then
                ListIntersectInternal list1 tail2 offset (head2::acc) (currentIndex + 1)
            else
                ListIntersectInternal tail1 list2 offset (head1::acc) (currentIndex + 1)

    let ListIntersect<'T> (list1: List<'T>) (list2: List<'T>) (offset: uint32): List<'T> =
        ListIntersectInternal list1 list2 offset [] 1

    let WithTimeout (timeSpan: TimeSpan) (job: Async<'R>): Async<Option<'R>> = async {
        let read = async {
            let! value = job
            return value |> SuccessfulValue |> Some
        }

        let delay = async {
            let total = int timeSpan.TotalMilliseconds
            do! Async.Sleep total
            return FailureResult <| TimeoutException() |> Some
        }

        let! dummyOption = Async.Choice([read; delay])
        match dummyOption with
        | Some theResult ->
            match theResult with
            | SuccessfulValue r ->
                return Some r
            | FailureResult _ ->
                return None
        | None ->
            // none of the jobs passed to Async.Choice returns None
            return failwith "unreachable"
    }

    // FIXME: we should not need this workaround anymore when this gets addressed:
    //        https://github.com/fsharp/fslang-suggestions/issues/660
    let ReRaise (ex: Exception): Exception =
        (ExceptionDispatchInfo.Capture ex).Throw ()
        failwith "Should be unreachable"
        ex

    let rec public FindException<'T when 'T:> Exception>(ex: Exception): Option<'T> =
        let rec findExInSeq(sq: seq<Exception>) =
            match Seq.tryHead sq with
            | Some head ->
                let found = FindException head
                match found with
                | Some ex -> Some ex
                | None ->
                    findExInSeq <| Seq.tail sq
            | None ->
                None
        if null = ex then
            None
        else
            match ex with
            | :? 'T as specificEx -> Some(specificEx)
            | :? AggregateException as aggEx ->
                findExInSeq aggEx.InnerExceptions
            | _ -> FindException<'T>(ex.InnerException)

    // Searches through an exception tree and ensures that all the leaves of
    // the tree have type 'T. Returns these 'T exceptions as a sequence, or
    // otherwise re-raises the original exception if there are any non-'T-based
    // exceptions in the tree.
    let public FindSingleException<'T when 'T:> Exception>(ex: Exception): seq<'T> =
        let rec findSingleExceptionOpt(ex: Exception): Option<seq<'T>> =
            let rec findSingleExceptionInSeq(sq: seq<Exception>)(acc: seq<'T>): Option<seq<'T>> =
                match Seq.tryHead sq with
                | Some head ->
                    match findSingleExceptionOpt head with
                    | Some exs ->
                        findSingleExceptionInSeq (Seq.tail sq) (Seq.concat [acc; exs])
                    | None -> None
                | None -> Some acc
            let findSingleInnerException(ex: Exception): Option<seq<'T>> =
                if null = ex.InnerException then
                    None
                else
                    findSingleExceptionOpt ex.InnerException
            match ex with
            | :? 'T as specificEx -> Some <| Seq.singleton specificEx
            | :? AggregateException as aggEx ->
                findSingleExceptionInSeq aggEx.InnerExceptions Seq.empty
            | _ -> findSingleInnerException ex
        match findSingleExceptionOpt ex with
        | Some exs -> exs
        | None ->
            ReRaise ex
            |> ignore<Exception>
            failwith "unreachable"

#if STRICTER_COMPILATION_BUT_WITH_REFLECTION_AT_RUNTIME
// http://stackoverflow.com/a/28466431/6503091
    // will crash if 'T contains members which aren't only tags
    let Construct<'T> (caseInfo: UnionCaseInfo) = FSharpValue.MakeUnion(caseInfo, [||]) :?> 'T

    let GetUnionCaseInfoAndInstance<'T> (caseInfo: UnionCaseInfo) = (Construct<'T> caseInfo)

    let GetAllElementsFromDiscriminatedUnion<'T>() =
        FSharpType.GetUnionCases(typeof<'T>)
        |> Seq.map GetUnionCaseInfoAndInstance<'T>
#endif

    type OptionBuilder() =
        // see https://github.com/dsyme/fsharp-presentations/blob/master/design-notes/ces-compared.md#overview-of-f-computation-expressions
        member x.Bind (v,f) = Option.bind f v
        member x.Return v = Some v
        member x.ReturnFrom o = o
        member x.Zero () = None

    let option = OptionBuilder()
