open System.IO


let words =
    [ "one"
      "two"
      "three"
      "four"
      "five"
      "six"
      "seven"
      "eight"
      "nine"
      "ten"
      "1"
      "2"
      "3"
      "4"
      "5"
      "6"
      "7"
      "8"
      "9" ]

let rec getFirstDigit (str: string) =
    if str.Length = 0 then
        None
    else
        match words |> List.tryFind (fun w -> str.StartsWith w) with
        | Some w -> Some w
        | None -> getFirstDigit str.[1..]

let rec getLastDigit (str: string) =
    if str.Length = 0 then
        None
    else
        match words |> List.tryFind (fun w -> str.EndsWith w) with
        | Some w -> Some w
        | None -> getLastDigit str.[..^1]

let wordToValue = function
    | "one" | "1" -> 1
    | "two" | "2" -> 2
    | "three" | "3" -> 3
    | "four" | "4" -> 4
    | "five" | "5" -> 5
    | "six" | "6" -> 6
    | "seven" | "7" -> 7
    | "eight" | "8" -> 8
    | "nine" | "9" -> 9
    | _ -> 0

let getCallibration (str: string) =
    let firstDigit = getFirstDigit str
    let lastDigit = getLastDigit str
    match firstDigit, lastDigit with
    | Some f, Some l -> Some (f, l)
    | _ -> None

let () =
    let lines = File.ReadAllLines "input.txt"

    lines
    |> Seq.map getCallibration
    |> Seq.choose id
    |> Seq.map (fun (f, l) -> (wordToValue f) * 10 + wordToValue l)
    |> Seq.sum
    |> printf "%d\n"
