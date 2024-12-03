open Day2.Parser.Parser
open Day2.Parser

// let input =
// "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let input = System.IO.File.ReadAllText("input.txt")

let mulParser =
    pstring "mul" >>. pchar '(' >>. manyDigit .>> pchar ',' .>>. manyDigit
    .>> pchar ')'

let parser = many (skipManyTill anyChar mulParser)

let result = run parser input

let evaluate =
    function
    | Success(value, _) -> Ok(value |> List.map (fun (a, b) -> a * b) |> List.sum)
    | Failure(msg) -> Result.Error msg

printfn "%A" <| evaluate result
