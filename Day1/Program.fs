open System.IO
open System
open Microsoft.FSharp.Core

let resultFromThrowable f a =
    try
        Ok(f (a))
    with ex ->
        Error ex

let readFile (filePath: string) =
    resultFromThrowable File.ReadAllLines filePath

let lines = (readFile "./input.txt")

let getCallibrationFromString (str: string) =
    let first = Seq.tryFind Char.IsDigit str
    let last = Seq.tryFindBack Char.IsDigit str

    match first, last with
    | Some first, Some last -> Some(first, last)
    | _ -> None

let processLines lines =
    lines
    |> Seq.map getCallibrationFromString
    |> Seq.map (function
        | Some(a, b) -> string a + string b
        | None -> "0")
    |> Seq.map (fun s ->
        match System.Int32.TryParse(s) with
        | (true, v) -> v
        | _ -> 0)
    |> Seq.sum


let result =
    result {
        let! lines = readFile "input.txt"
        return processLines lines
    }


let example = "treb7uchet"

match result with
| Ok result -> printfn "%d" result
| Error ex -> printfn "Error: %s" ex.Message

printfn "%d" (processLines [ example ])
