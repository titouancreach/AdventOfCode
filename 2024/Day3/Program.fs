open Day2.Parser.Parser
open Day2.Parser

// let input =
// "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

type Token =
    | Mul of int * int
    | Do
    | Dont

let input = System.IO.File.ReadAllText("input.txt")

let mulParser =
    pstring "mul" >>. pchar '(' >>. manyDigit .>> pchar ',' .>>. manyDigit
    .>> pchar ')'
    |>> (fun (a, b) -> Mul(a, b))

let doParser = pstring "do()" |>> (fun _ -> Do)
let dontParser = pstring "don't()" |>> (fun _ -> Dont)

let parser =
    many (skipManyTill anyChar (choice [ mulParser; dontParser; doParser ]))

let result = run parser input

let evaluatePartOne =
    function
    | Success(x, _) ->
        Ok(
            List.map
                (function
                | Mul(a, b) -> Some(a * b)
                | _ -> None)

                x
        )
    | _ -> Error("Failed to parse")


let removeDont (tokens: Token list) : Token list =
    let rec loop tokens enabled acc =
        match tokens with
        | [] -> List.rev acc
        | Dont :: tail -> loop tail false acc
        | Do :: tail -> loop tail true acc
        | Mul(a, b) :: tail when enabled -> loop tail enabled (Mul(a, b) :: acc)
        | _ :: tail -> loop tail enabled acc

    loop tokens true []


let evaluatePartTwo =
    function
    | Success(x, _) ->
        Ok(
            x
            |> removeDont
            |> List.map (function
                | Mul(a, b) -> Some(a * b)
                | _ -> None)

        )
    | _ -> Error("Failed to parse")



let x =
    evaluatePartOne result |> Result.map (List.choose id) |> Result.map (List.sum)

let y =
    evaluatePartTwo result |> Result.map (List.choose id) |> Result.map (List.sum)

printfn "%A" x
printfn "%A" y
