open System.Collections.Generic

let inputEx1 =
    [| "...0..."
       "...1..."
       "...2..."
       "6543456"
       "7.....7"
       "8.....8"
       "9.....9" |]

let inputEx2 =
    [| "..90..9"
       "...1.98"
       "...2..7"
       "6543456"
       "765.987"
       "876...."
       "987...." |]

let largerInput =
    [| "89010123"
       "78121874"
       "87430965"
       "96549874"
       "45678903"
       "32019012"
       "01329801"
       "10456732" |]

let puzzleInput = System.IO.File.ReadAllLines "input.txt"

let parse (input: string array) =
    input |> Array.map _.ToCharArray() |> array2D

let findTrailHead (map: char[,]) =
    seq {
        for i in 0 .. map.GetLength(0) - 1 do
            for j in 0 .. map.GetLength(1) - 1 do
                if map[i, j] = '0' then
                    yield (i, j)
    }

let tryGetChar (map: char[,]) ((i, j)) =
    if i >= 0 && i < map.GetLength(0) && j >= 0 && j < map.GetLength(1) then
        Some map[i, j]
    else
        None

let solve (map: char[,]) (trailHead: (int * int)) =
    let visitedNines = HashSet<int * int>()

    let rec aux currentCount (currentPos: int * int) (path: (int * int) list) =
        match tryGetChar map currentPos with
        | Some '9' when currentCount = 9 -> //&& not (visitedNines.Contains(currentPos)) ->
            visitedNines.Add(currentPos) |> ignore
            1
        | Some c when int (c - '0') = currentCount ->
            let left =
                aux (currentCount + 1) (fst currentPos, snd currentPos - 1) (currentPos :: path)

            let right =
                aux (currentCount + 1) (fst currentPos, snd currentPos + 1) (currentPos :: path)

            let up =
                aux (currentCount + 1) (fst currentPos - 1, snd currentPos) (currentPos :: path)

            let down =
                aux (currentCount + 1) (fst currentPos + 1, snd currentPos) (currentPos :: path)

            left + right + up + down

        | _ -> 0

    aux 0 trailHead []

let map = parse puzzleInput

let resultPart1 = map |> findTrailHead |> Seq.map (solve map) |> Seq.sum

printfn "%A" resultPart1
