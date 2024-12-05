// let input =
//     [ "....XXMAS."
//       ".SAMXMS..."
//       "...S..A..."
//       "..A.A.MS.X"
//       "XMASAMX.MM"
//       "X.....XA.A"
//       "S.S.S.S.SS"
//       ".A.A.A.A.A"
//       "..M.M.M.MM"
//       ".X.X.XMASX" ]

let input = System.IO.File.ReadAllLines "input.txt" |> List.ofArray

type Matrix = char array array

let tryGet (input: Matrix) (x: int) (y: int) =
    if x >= 0 && x < input.[0].Length && y >= 0 && y < input.Length then
        Some input.[y].[x]
    else
        None

let getDirectionLists (position: int) (input: Matrix) =
    let x = position % input.[0].Length
    let y = position / input.[0].Length

    let topLeft = seq { for i in 0..3 -> tryGet input (x - i) (y - i) }
    let top = seq { for i in 0..3 -> tryGet input x (y - i) }
    let topRight = seq { for i in 0..3 -> tryGet input (x + i) (y - i) }
    let right = seq { for i in 0..3 -> tryGet input (x + i) y }
    let bottomRight = seq { for i in 0..3 -> tryGet input (x + i) (y + i) }
    let bottom = seq { for i in 0..3 -> tryGet input x (y + i) }
    let bottomLeft = seq { for i in 0..3 -> tryGet input (x - i) (y + i) }
    let left = seq { for i in 0..3 -> tryGet input (x - i) y }

    [ topLeft; top; topRight; right; bottomRight; bottom; bottomLeft; left ]


// if we find a A then take upper left, upper right, lower left, lower right
let getDirectionListsPart2 (position: int) (input: Matrix) =
    let x = position % input.[0].Length
    let y = position / input.[0].Length

    let current = tryGet input x y
    let topLeft = tryGet input (x - 1) (y - 1)
    let topRight = tryGet input (x + 1) (y - 1)
    let bottomRight = tryGet input (x + 1) (y + 1)
    let bottomLeft = tryGet input (x - 1) (y + 1)

    ([| topLeft; current; bottomRight |], [| topRight; current; bottomLeft |])


let lift2 (op: 'a -> 'a -> bool) (opt1: 'a option) (opt2: 'a option) : bool =
    match opt1, opt2 with
    | Some x, Some y -> op x y
    | None, None -> true
    | _ -> false

let isXmas (seqToCheck: char option seq) : bool =
    let target = seq [ Some 'X'; Some 'M'; Some 'A'; Some 'S' ]
    let targetLength = Seq.length target
    let srcLength = Seq.length seqToCheck
    let lengthEq = srcLength = targetLength
    lengthEq && Seq.forall2 (lift2 (=)) seqToCheck target

let countXmas (directions: char option seq list) =
    directions |> List.filter isXmas |> List.length

let getXmasCountAtPosition (position: int) =
    (getDirectionLists position >> countXmas)

let result =
    let matrice = input |> List.map (fun x -> x.ToCharArray()) |> Seq.toArray
    let totalLenght = matrice.Length * matrice.[0].Length

    let xmaxCount =
        seq { for i in 0 .. totalLenght - 1 -> getXmasCountAtPosition i matrice }

    Seq.sum xmaxCount


let isXmas2 =
    function
    | (a, b) when (a = "MAS" || a = "SAM") && (b = "MAS" || b = "SAM") -> true
    | _ -> false

let result2 =
    let matrice = input |> List.map (fun x -> x.ToCharArray()) |> Seq.toArray
    let totalLenght = matrice.Length * matrice.[0].Length

    let joinArrayOfOption (x: char option array) =
        x |> Array.choose (fun x -> x) |> System.String

    let diags =
        seq {
            for i in 0 .. totalLenght - 1 ->
                getDirectionListsPart2 i matrice
                |> fun (a, b) -> (joinArrayOfOption a, joinArrayOfOption b) // convert to string
                |> fun (a, b) ->
                    printfn ("%s %s") a b
                    (a, b)
                |> isXmas2
        }

    diags |> Seq.filter id |> Seq.length



printfn "%A" result
printfn "%A" result2
