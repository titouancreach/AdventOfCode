open System.Collections.Generic

type Stone = Stone of bigint

let counter (stones: bigint seq) : IDictionary<Stone, bigint> =
    stones
    |> Seq.map Stone
    |> Seq.groupBy id
    |> Seq.map (fun (stone, group) -> stone, bigint (Seq.length group))
    |> dict

let nextStep (stone: Stone) : Stone list =
    match stone with
    | Stone n when n = 0I -> [ Stone 1I ]
    | Stone n ->
        let digits = n.ToString()
        let len = digits.Length
        let half = len / 2
        let remainder = len % 2

        if remainder = 0 then
            [ Stone(bigint.Parse digits.[.. half - 1])
              Stone(bigint.Parse digits.[half..]) ]
        else
            [ Stone(n * 2024I) ]

let part2 (puzzleInput: string) =
    let stones = puzzleInput.Split() |> Seq.map bigint.Parse |> counter
    let mutable stonesDict = stones

    for _ in 1..75 do
        let newStones = Dictionary<Stone, bigint>()

        for kvp in stonesDict do
            let stone, count = kvp.Key, kvp.Value

            for child in nextStep stone do
                if newStones.ContainsKey child then
                    newStones.[child] <- newStones.[child] + count
                else
                    newStones.[child] <- count

        stonesDict <- newStones

    // Return the sum of all counts
    stonesDict.Values |> Seq.sum

// Example usage
let input = "0 7 198844 5687836 58 2478 25475 894"
printfn "%A" (part2 input)
