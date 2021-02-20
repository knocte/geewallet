namespace GWallet.Backend

type UnsafeRandom = System.Random

module Shuffler =

    let private random = UnsafeRandom ()

    let Unsort aSeq =
        aSeq |> Seq.sortBy (fun _ -> random.Next ())

    let private ListRemove<'T when 'T: equality>
        (list: List<'T>)
        (elementToRemove: 'T)
        =
        List.filter (fun element -> element <> elementToRemove) list

    let RandomizeEveryNthElement<'T when 'T: equality>
        (list: List<'T>)
        (offset: uint32)
        =
        let rec RandomizeInternal
            (list: List<'T>)
            (offset: uint32)
            acc
            (currentIndex: uint32)
            =
            match list with
            | [] -> List.rev acc
            | head :: tail ->
                let nextIndex = (currentIndex + 1u)

                if currentIndex % offset <> 0u || tail = [] then
                    RandomizeInternal tail offset (head :: acc) nextIndex
                else
                    let randomizedRest = Unsort tail |> List.ofSeq

                    match randomizedRest with
                    | [] ->
                        failwith "should have fallen under previous 'if' case"
                    | randomizedHead :: _ ->
                        let newRest = head :: (ListRemove tail randomizedHead)

                        RandomizeInternal
                            newRest
                            offset
                            (randomizedHead :: acc)
                            nextIndex

        RandomizeInternal list offset [] 1u
