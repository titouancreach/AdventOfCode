printfn "Hello from F#"

let input =
    [| "....#....."
       ".........#"
       ".........."
       "..#......."
       ".......#.."
       ".........."
       ".#..^....."
       "........#."
       "#........."
       "......#..." |]

let puzzleInput = System.IO.File.ReadAllLines("input.txt")

type Map =
    { Matrix: char[,]
      Width: int
      Height: int }

type Direction =
    | Up
    | Down
    | Left
    | Right

type Position =
    { X: int; Y: int; Direction: Direction }

let parseMap (input: string array) : Map =
    let width = input.[0].Length in
    let height = input.Length in
    let matrix = input |> Array.map (fun x -> x.ToCharArray()) |> array2D

    { Matrix = matrix
      Width = width
      Height = height }

let getInitialPosition (map: Map) : Position =
    seq {
        for y in 0 .. map.Height - 1 do
            for x in 0 .. map.Width - 1 do
                match map.Matrix.[y, x] with
                | '^' -> yield { X = x; Y = y; Direction = Up }
                | 'v' -> yield { X = x; Y = y; Direction = Down }
                | '<' -> yield { X = x; Y = y; Direction = Left }
                | '>' -> yield { X = x; Y = y; Direction = Right }
                | _ -> ()
    }
    |> Seq.head

let getNextDirection =
    function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up


let tryGetCharAt (map: Map) (x: int) (y: int) : char option =
    if x >= 0 && x < map.Width && y >= 0 && y < map.Height then
        Some(map.Matrix.[y, x])
    else
        None

let rec getNextPosition (map: Map) (guardPosition: Position) : Position option =
    let nextMatrixPosition =
        match guardPosition with
        | { Direction = Up; X = x; Y = y } -> (x, y - 1)
        | { Direction = Down; X = x; Y = y } -> (x, y + 1)
        | { Direction = Left; X = x; Y = y } -> (x - 1, y)
        | { Direction = Right; X = x; Y = y } -> (x + 1, y)

    let (nextX, nextY) = nextMatrixPosition
    let charAtNextPosition = tryGetCharAt map nextX nextY

    match charAtNextPosition with
    | Some('#' | 'O') -> // wall
        getNextPosition
            map
            { guardPosition with
                Direction = getNextDirection guardPosition.Direction }

    | Some _ -> // free position
        Some
            { guardPosition with
                X = nextX
                Y = nextY }

    | _ -> None


let getPositionSeq (map: Map) (initialPosition: Position) =
    seq {
        yield initialPosition

        yield!
            Seq.unfold
                (fun currentPosition ->
                    match getNextPosition map currentPosition with
                    | Some nextPosition -> Some(nextPosition, nextPosition)
                    | None -> None)
                initialPosition
    }


let part1 =
    let map = parseMap puzzleInput in
    let initialPosition = getInitialPosition map
    let positionSeq = getPositionSeq map initialPosition

    // count distincts pairs of position
    let count =
        positionSeq |> Seq.map (fun x -> (x.X, x.Y)) |> Seq.distinct |> Seq.length

    printfn "count: %d" count


let isCyclic positionSeq =
    let seen = System.Collections.Generic.HashSet<Position>()

    positionSeq
    |> Seq.exists (fun pos ->
        if seen.Contains(pos) then
            true
        else
            seen.Add(pos) |> ignore
            false)

let part2 =
    let map = parseMap puzzleInput
    let initialPosition = getInitialPosition map

    // bruteforce like there is no tomorrow
    let checkForLoop x y map initialPosition =
        if map.Matrix.[y, x] = '.' then
            map.Matrix.[y, x] <- 'O'

            let positionSeq = (getPositionSeq map initialPosition) |> Seq.truncate 10000 //// lol
            let cyclic = isCyclic positionSeq

            if cyclic then
                printfn "x: %d, y: %d, cyclic: %b" x y cyclic

            map.Matrix.[y, x] <- '.'

            (x, y, cyclic)
        else
            (x, y, false) // No need to check if it's not a '.'

    let tasks =
        [ for y in 0 .. map.Height - 1 do
              for x in 0 .. map.Width - 1 do
                  yield checkForLoop x y map initialPosition ]

    let results = tasks |> List.toArray

    let count =
        results
        |> Array.distinct
        |> Array.sumBy (fun (x, y, cyclic) -> if cyclic then 1 else 0)

    printfn "Total cyclic count: %d" count
