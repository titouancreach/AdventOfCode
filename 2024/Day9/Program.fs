// For more information see https://aka.ms/fsharp-console-apps

let input = "2333133121414131402"

let input2 = System.IO.File.ReadLines("input.txt") |> Seq.head

type Id = Id of int

type FilledRecord = { count: int; id: Id }

type Bloc =
    | Filled of FilledRecord
    | Free of int

    override this.ToString() : string =
        match this with
        | Filled({ count = count; id = Id(id) }) -> sprintf "Filled(%d)*%d" count id
        | Free(n) -> sprintf "Free*%d" n

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




let countAll (fileSystem: Bloc list) =
    seq {
        for (i, bloc) in fileSystem |> Seq.indexed do
            let subSeq =
                match bloc with
                | Filled({ count = count; id = Id(id) }) -> Seq.replicate count id
                | Free(n) -> Seq.replicate n 0

            yield! subSeq
    }


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




let result_part1 (fileSystem: Bloc list) =
    fileSystem
    |> part1
    |> countAll
    |> Seq.toList
    |> Seq.indexed
    |> Seq.sumBy (fun (i, id) -> bigint id * bigint i)

let part2 (fileSystem: Bloc list) =
    let reversed = (List.rev fileSystem)

    let rec aux =
        function
        | [] -> []
        // free blocs don't need to be reallocated
        | (Free n) :: xs -> (Free n) :: aux xs
        | (Filled(filledItem)) :: xs ->

            let lastFreeSpaceLargeEnough =
                Seq.tryFindBack
                    (function
                    | (_, Free(n)) when n >= filledItem.count -> true
                    | _ -> false)
                    (xs |> List.indexed)

            match lastFreeSpaceLargeEnough with
            | Some(index, Free(freeItem)) ->

                let freeOrNothing =
                    if (freeItem - filledItem.count) = 0 then
                        []
                    else
                        [ Free(freeItem - filledItem.count) ]

                let before = xs.[0 .. index - 1]
                let after = xs.[index + 1 ..]

                Free(filledItem.count)
                :: aux (before @ freeOrNothing @ [ Filled filledItem ] @ after)
            | _ -> Filled(filledItem) :: aux xs

    aux reversed |> List.rev


let part2TailRec (fileSystem: Bloc list) =
    let reversed = List.rev fileSystem

    let rec aux acc remaining =
        match remaining with
        | [] -> acc
        // Free blocks are added to the accumulator as they are
        | (Free n) :: xs -> aux (Free n :: acc) xs
        | (Filled filledItem) :: xs ->
            let lastFreeSpaceLargeEnough =
                Seq.tryFindBack
                    (function
                    | (_, Free n) when n >= filledItem.count -> true
                    | _ -> false)
                    (xs |> List.indexed)

            match lastFreeSpaceLargeEnough with
            | Some(index, Free freeItem) ->
                let freeOrNothing =
                    if (freeItem - filledItem.count) = 0 then
                        []
                    else
                        [ Free(freeItem - filledItem.count) ]

                let before = xs.[0 .. index - 1]
                let after = xs.[index + 1 ..]

                // Accumulate the new Free block and recursively process the rest
                aux (Free filledItem.count :: acc) (before @ freeOrNothing @ [ Filled filledItem ] @ after)
            | _ ->
                // If no suitable free space is found, keep the filled block
                aux (Filled filledItem :: acc) xs

    aux [] reversed


let printFileSystem (fileSystem: Bloc list) =
    let myseq =
        seq {
            for (bloc) in fileSystem do
                let subSeq =
                    match bloc with
                    | Filled({ count = count; id = Id(id) }) -> Seq.replicate count (string id)
                    | Free(n) -> Seq.replicate n "."

                yield! subSeq
        }

    myseq |> Seq.iter (fun id -> printf "%s" id) |> (fun _ -> printfn "")


let result_part2 (fileSystem: Bloc list) =
    fileSystem
    |> part2TailRec
    |> countAll
    |> Seq.toList
    |> Seq.indexed
    |> Seq.sumBy (fun (i, id) -> bigint id * bigint i)

let fileSystem = parse input2

printfn "Before"
printfn "Part 2: %A" (result_part2 fileSystem)
