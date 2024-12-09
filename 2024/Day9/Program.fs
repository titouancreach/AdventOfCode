// For more information see https://aka.ms/fsharp-console-apps

let input = "2333133121414131402"

let input2 = System.IO.File.ReadLines("input.txt") |> Seq.head

type Id = Id of int

type FilledRecord = { count: int; id: Id }

type Bloc =
    | Filled of FilledRecord
    | Free of int

let parse (input: string) =
    input
    |> Seq.mapi (fun i c ->
        if i % 2 <> 0 then
            Free(int (c - '0'))
        else
            Filled(
                { count = int (c - '0')
                  id = Id(i / 2) }
            ))
    |> Seq.toList


let rec part1 (fileSystem: Bloc list) =

    let indexed = Seq.indexed fileSystem


    let tryFindNextFreeBloc =
        Seq.tryFind (function
            | (_, Free(_)) -> true
            | _ -> false)

    let tryFindLastFilledBloc =
        Seq.tryFindBack (function
            | (_, Filled(_)) -> true
            | _ -> false)

    let nextFreeBloc = tryFindNextFreeBloc indexed
    let lastFilledBloc = tryFindLastFilledBloc indexed

    match (nextFreeBloc, lastFilledBloc) with
    | (Some(freeIndex, _), Some(filledIndex, _)) when filledIndex < freeIndex -> fileSystem
    | (Some(freeIndex, Free(freeItem)), Some(filledIndex, Filled(filledItem))) ->
        let blocToTake = min freeItem filledItem.count

        // the bloc that will replace the first free bloc
        let first =
            Filled
                { count = blocToTake
                  id = filledItem.id }

        // the bloc that will replace the last filled bloc
        let last = Free(blocToTake)

        // if the filled bloc didn't take all the free bloc
        let maybeRemainingFree: Bloc list =
            if freeItem > blocToTake then
                [ first; Free(freeItem - blocToTake) ]
            else
                [ first ]

        // if the filled bloc didn't take all the free bloc
        let maybeRemainingFilled: Bloc list =
            if filledItem.count > blocToTake then
                [ last
                  Filled
                      { count = filledItem.count - blocToTake
                        id = filledItem.id } ]
            else
                [ last ]

        let newFileSystem =
            fileSystem[0 .. freeIndex - 1]
            @ maybeRemainingFree
            @ fileSystem[freeIndex + 1 .. filledIndex - 1]
            @ maybeRemainingFilled
            @ fileSystem[filledIndex + 1 ..]

        part1 newFileSystem

    | _ -> fileSystem


let countAll (fileSystem: Bloc list) =
    seq {
        for (i, bloc) in fileSystem |> Seq.indexed do
            let subSeq =
                match bloc with
                | Filled({ count = count; id = Id(id) }) -> seq { for _ in 1..count -> id }
                | Free(_) -> Seq.empty

            yield! subSeq
    }



printfn
    "%A"
    (parse input2
     |> part1
     |> countAll
     |> Seq.toList
     |> Seq.indexed
     |> Seq.sumBy (fun (i, id) -> bigint id * bigint i))
