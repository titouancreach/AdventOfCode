open Day2.Parser
open Day2.Parser.Parser


type TestValue = TestValue of bigint
type Values = Values of bigint list

// let input =
//     [| "190: 10 19"
//        "3267: 81 40 27"
//        "83: 17 5"
//        "156: 15 6"
//        "7290: 6 8 6 15"
//        "161011: 16 10 13"
//        "192: 17 8 14"
//        "21037: 9 7 18 13"
//        "292: 11 6 16 20" |]

let input = System.IO.File.ReadAllLines "./input.txt"

let parse (input: string) =
    let parser =
        (manyDigit |>> TestValue) .>> pchar ':'
        .>>. (many1 (whitespaceChar >>. manyDigit) |>> Values)

    match run parser input with
    | Success(value, "") -> value
    | Failure msg -> failwith msg
    | _ -> failwith "Unexpected result"

let concatBigint a b = string a + string b |> bigint.Parse

let operators = [ ("+", (+)); ("*", (*)); ("||", concatBigint) ]

let findOperator (input: (TestValue * Values)) =
    let (TestValue target, Values numbers) = input

    let rec backtrack result remainingNumbers path =
        match remainingNumbers with
        | [] -> if result = target then Some(path) else None
        | head :: rest ->
            operators
            |> List.tryPick (fun (opSymbol, fn) ->
                let newResult = fn result head
                backtrack newResult rest (path @ [ opSymbol ]))

    match numbers with
    | first :: rest -> backtrack first rest []
    | [] -> None

let result1 =
    let result = input |> Array.map parse

    let result =
        result
        |> Array.map (fun x -> (x, findOperator x))
        |> Array.choose (fun ((initialData, path)) ->
            match path with
            | Some path ->
                let (TestValue target, Values numbers) = initialData
                Some(target)
            | None -> None)

    printfn "%A" <| Array.sum result
