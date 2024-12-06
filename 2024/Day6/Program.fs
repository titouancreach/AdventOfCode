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
    | Some('#') -> // wall
        getNextPosition
            map
            { guardPosition with
                Direction = getNextDirection guardPosition.Direction }

    | Some(c) -> // free position
        Some
            { guardPosition with
                X = nextX
                Y = nextY }

    | _ -> None

let result =
    let map = parseMap puzzleInput in
    let initialPosition = getInitialPosition map

    let positionSeq =
        seq {
            yield initialPosition

            yield!
                Seq.unfold
                    (fun currentPosition ->
                        match getNextPosition map currentPosition with
                        | Some(nextPosition) -> Some(nextPosition, nextPosition)
                        | None -> None)
                    initialPosition
        }

    printfn "Initial position: x: %d, y: %d" initialPosition.X initialPosition.Y

    for position in positionSeq do
        printfn "x: %d, y: %d" position.X position.Y

    // count distincts pairs of position
    let count =
        positionSeq |> Seq.map (fun x -> (x.X, x.Y)) |> Seq.distinct |> Seq.length

    printfn "count: %d" count
