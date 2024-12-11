open System.Collections.Generic
type Stone = Stone of bigint

let ex1 = "125 17" |> _.Split(" ") |> Array.map (bigint.Parse >> Stone)

let puzzle =
    "0 7 198844 5687836 58 2478 25475 894"
    |> _.Split(" ")
    |> Array.map (bigint.Parse >> Stone)

let memoize f =
    let dict = Dictionary<_, _>()

    fun c ->
        let exist, value = dict.TryGetValue c

        match exist with
        | true -> value
        | _ ->
            let value = f c
            dict.Add(c, value)
            value

let hasEvenDigits (n: bigint) =
    let rec countDigits n count =
        if n = 0I then count else countDigits (n / 10I) (count + 1)

    (countDigits n 0) % 2 = 0

let splitInHalf (n: bigint) =
    let str = string n
    let half = str.Length / 2
    str.Substring(0, half), str.Substring(half)


let blink =
    function
    | Stone n when n = 0I -> [| Stone 1I |]
    | Stone n when hasEvenDigits n ->
        let halfs = splitInHalf n
        [| Stone(bigint.Parse(fst halfs)); Stone(bigint.Parse(snd halfs)) |]
    | Stone n -> [| Stone(n * bigint 2024) |]


let mutable count = 0

let memoizedBlink = memoize blink

let blinks =
    let generateSeq =
        Seq.unfold (fun sequence ->
            printfn "Calculating sequence number %d" (count)
            count <- count + 1
            let newSeq = Array.collect memoizedBlink sequence
            Some(sequence, Array.collect memoizedBlink sequence))

    generateSeq (puzzle) |> Seq.take 76 // nb of blink if (+ 1) becasue the first state is part of the sequence, so for 6 blinks, we need to take 7

(blinks |> Seq.toList |> List.rev |> List.head |> Array.length |> printfn "%d")
