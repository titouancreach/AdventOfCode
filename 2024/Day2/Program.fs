// For more information see https://aka.ms/fsharp-console-apps


// let input =
//     [ "7 6 4 2 1"; "1 2 7 8 9"; "9 7 6 2 1"; "1 3 2 4 5"; "8 6 4 4 1"; "1 3 6 7 9" ]

let input = System.IO.File.ReadAllLines("input.txt") |> Seq.toList


type LastNumber = LastNumber of int

type Trending =
    | Increasing of LastNumber
    | Decreasing of LastNumber
    | Initial of LastNumber

type Validity =
    | Valid of Trending
    | Invalid
    | Initial

let isSafe (a: int array) =
    Array.fold
        (fun acc x ->
            match acc with
            | Initial -> Valid(Trending.Initial(LastNumber(x)))
            | Invalid -> Invalid
            | Valid trend ->
                match trend with
                | Increasing(LastNumber(last)) when x > last && abs (last - x) <= 3 -> Valid(Increasing(LastNumber(x)))
                | Decreasing(LastNumber(last)) when x < last && abs (last - x) <= 3 -> Valid(Decreasing(LastNumber(x)))
                | Trending.Initial(LastNumber(last)) when abs (x - last) <= 3 && abs (x - last) > 0 ->
                    if x > last then
                        Valid(Increasing(LastNumber(x)))
                    else
                        Valid(Decreasing(LastNumber(x)))
                | _ -> Invalid)

        Validity.Initial
        a

let result1 =
    input
    |> List.map (fun x -> x.Split(' ') |> Array.map int)
    |> List.map isSafe
    |> List.choose (function
        | Valid _ -> Some()
        | _ -> None)
    |> List.length

let isSafeBool (arr: int array) =
    match isSafe arr with
    | Valid _ -> true
    | _ -> false


let generateArrays (arr: int array) =
    arr
    |> Array.mapi (fun idx _ ->
        Array.append (Array.sub arr 0 idx) (Array.sub arr (idx + 1) (Array.length arr - idx - 1)))

let trySafe (arr: int array) =
    if isSafe arr <> Invalid then
        Some arr
    else
        arr |> generateArrays |> Array.tryFind isSafeBool

let result2 =
    input
    |> List.map (fun x -> x.Split(' ') |> Array.map int)
    |> List.map trySafe
    |> List.choose id
    |> List.length


printfn "%A" result1
printfn "%A" result2
