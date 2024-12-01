open Day2.Parser
open Day2.Parser.Parser

// let input = [ "3   4"; "4   3"; "2   5"; "1   3"; "3   9"; "3   3" ]
let input = System.IO.File.ReadAllLines("input.txt")

let parseline = ((manyDigit .>> whitespace) .>>. manyDigit)

let part1 =
    input
    |> Seq.toList
    |> List.map (fun item -> (run parseline item))
    |> List.choose (function
        | Success(value, "") -> Some(value)
        | _ -> None)
    |> List.unzip
    |> (fun (a, b) -> (List.sort a, List.sort b))
    |> (fun (a, b) -> List.map2 (fun x y -> abs <| x - y) a b)
    |> List.sum

let part2 =
    input
    |> Seq.toList
    |> List.map (fun item -> (run parseline item))
    |> List.map (function
        | Success(value, "") -> Some(value)
        | _ -> None)
    |> List.choose id
    |> List.unzip
    |> (fun (a, b) ->
        List.fold
            (fun acc elem ->
                let eq = (=)
                let occurence = List.filter (eq elem) b |> List.length
                acc + (occurence * elem))
            0
            a)

printfn "%A" part1
printfn "%A" part2
