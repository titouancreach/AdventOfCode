let inputGrid =
    array2D
        [ [ '4'; '6'; '7'; '.'; '.'; '1'; '1'; '4'; '.'; '.' ]
          [ '.'; '.'; '.'; '*'; '.'; '.'; '.'; '.'; '.'; '.' ]
          [ '.'; '.'; '3'; '5'; '.'; '.'; '6'; '3'; '3'; '.' ]
          [ '.'; '.'; '.'; '.'; '.'; '.'; '#'; '.'; '.'; '.' ]
          [ '6'; '1'; '7'; '*'; '.'; '.'; '.'; '.'; '.'; '.' ]
          [ '.'; '.'; '.'; '.'; '.'; '+'; '.'; '5'; '8'; '.' ]
          [ '.'; '.'; '5'; '9'; '2'; '.'; '.'; '.'; '.'; '.' ]
          [ '.'; '.'; '.'; '.'; '.'; '.'; '7'; '5'; '5'; '.' ]
          [ '.'; '.'; '.'; '$'; '.'; '*'; '.'; '.'; '.'; '.' ]
          [ '.'; '6'; '6'; '4'; '.'; '5'; '9'; '8'; '.'; '.' ] ]

// let inputGrid =
//     System.IO.File.ReadAllLines("input.txt")
//     |> Array.map (fun x -> x.ToCharArray())
//     |> array2D


// when we read 345 for example:
// - we first read 3 -> if 3 is next to a symbol, the acc is NextToASymbol "3" else it's NotNextToASymbol "3"
// - then we read 4, if 4 is next to a symbol, our acc becomes NextToASymbol "34" whatever if 3 was next to a symbol or not
type Acc =
    | NextToASymbol of string
    | NotNextToASymbol of string

let startAcc = NotNextToASymbol ""

let lift2 op a b =
    match a, b with
    | NotNextToASymbol a, NotNextToASymbol b -> NotNextToASymbol(op a b)
    | NotNextToASymbol a, NextToASymbol b -> NextToASymbol(op a b)
    | NextToASymbol a, NotNextToASymbol b -> NextToASymbol(op a b)
    | NextToASymbol a, NextToASymbol b -> NextToASymbol(op a b)

let addAcc = lift2 (+)
let width = Array2D.length1 inputGrid
let height = Array2D.length2 inputGrid

let charIsASymbol =
    function
    | c when '0' <= c && c <= '9' -> false
    | '.' -> false
    | _ -> true

let isNumber =
    function
    | c when '0' <= c && c <= '9' -> true
    | _ -> false

let isNextToSymbol x y (grid: char array2d) =
    let neighbors =
        [ (x - 1, y - 1)
          (x - 1, y)
          (x - 1, y + 1)
          (x, y - 1)
          (x, y + 1)
          (x + 1, y - 1)
          (x + 1, y)
          (x + 1, y + 1) ]

    neighbors
    |> List.filter (fun (nx, ny) -> nx >= 0 && ny >= 0 && nx < width && ny < height)
    |> List.exists (fun (nx, ny) -> charIsASymbol grid.[nx, ny])

let isNumberAndNotNextToASymbol x y (inputGrid: char array2d) =
    isNumber inputGrid.[x, y] && not (isNextToSymbol x y inputGrid)

let rec iterateGrid (inputGrid: char array2d) (position: int) (acc: Acc) (sum: int) =
    let x = position / width
    let y = position % width

    if position >= (width * height) then
        sum

    else if (isNumber inputGrid.[x, y]) then

        // end of number if it's the last element of a row or if next is not a number
        let isEndOfNumber x y =
            y = width - 1 || not (isNumber inputGrid.[x, y + 1])

        // check if the current element is next to a symbol
        let currentIsNextToASymbol = isNextToSymbol x y inputGrid

        let currentAcc =
            if currentIsNextToASymbol then
                addAcc acc (NextToASymbol(string inputGrid.[x, y]))
            else
                addAcc acc (NotNextToASymbol(string inputGrid.[x, y]))

        // if it's end of a number, just flush the acc
        if isEndOfNumber x y then
            let newSum =
                match currentAcc with
                | NextToASymbol s -> sum + int s
                | NotNextToASymbol _ -> sum

            printfn
                "Flushing currentAcc: %s"
                (match currentAcc with
                 | NextToASymbol s -> s
                 | NotNextToASymbol s -> s)

            iterateGrid inputGrid (position + 1) startAcc newSum
        else
            iterateGrid inputGrid (position + 1) (currentAcc) sum

    else
        iterateGrid inputGrid (position + 1) startAcc sum

printfn "final sum %d" (iterateGrid inputGrid 0 startAcc 0)
