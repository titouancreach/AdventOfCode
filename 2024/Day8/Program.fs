let input =
    System.IO.File.ReadAllLines("input.txt") |> Array.map _.ToCharArray() |> array2D

let findAntena (map: char[,]) =
    seq {
        for i in 0 .. map.GetLength(0) - 1 do
            for j in 0 .. map.GetLength(1) - 1 do
                match map.[i, j] with
                | c when c >= 'a' && c <= 'z' -> yield (c, i, j)
                | c when c >= '0' && c <= '9' -> yield (c, i, j)
                | c when c >= 'A' && c <= 'Z' -> yield (c, i, j)
                | _ -> ()
    }

let calculateAntinodes (positions: (int * int) list) =
    seq {
        for (x1, y1) in positions do
            for (x2, y2) in positions do
                // Vérifie que ce ne sont pas la même antenne
                if (x1, y1) <> (x2, y2) then
                    let dx = x2 - x1
                    let dy = y2 - y1
                    // Ajoute les deux antinœuds possibles
                    yield (x2 + dx, y2 + dy)
                    yield (x1 - dx, y1 - dy)
    }

let mapBounds (map: char[,]) x y =
    x >= 0 && x < map.GetLength(0) && y >= 0 && y < map.GetLength(1)

let traverse (map: char[,]) (x, y) (dx, dy) =
    Seq.unfold
        (fun (x, y) ->
            if mapBounds map x y then
                Some((x, y), (x + dx, y + dy))
            else
                None)
        (x, y)

let calculateAntinodes2 (map: char[,]) (positions: (int * int) list) =
    seq {
        for (x1, y1) in positions do
            for (x2, y2) in positions do
                // Vérifie que ce ne sont pas la même antenne
                if (x1, y1) <> (x2, y2) then
                    let dx = x2 - x1
                    let dy = y2 - y1

                    let seq1 = traverse map (x2, y2) (dx, dy)
                    let seq2 = traverse map (x1, y1) (-dx, -dy)

                    yield! seq1
                    yield! seq2
    }

let antenas = findAntena input |> Seq.groupBy (fun (c, _, _) -> c)

let calculateAllAntinodes (map: char[,]) =
    antenas
    |> Seq.collect (fun (c, positions) ->
        printfn "Calculating antinodes for %c" c

        positions
        |> Seq.map (fun (_, x, y) -> (x, y))
        |> Seq.toList
        |> (fun x ->
            printfn "Positions %A" x
            x)
        |> calculateAntinodes2 map
        |> (fun x ->
            printfn "Antinodes for %c: %A" c (x |> Seq.toList)
            x))
    |> Seq.distinct

let uniqueAntinodes = calculateAllAntinodes input

printfn "Part 2: %A" (uniqueAntinodes |> Seq.toList)
printfn "Part 2: %A" (uniqueAntinodes |> Seq.length)
