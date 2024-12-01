open Day2.Parser.Parser
open Day2.Parser

type Color =
    | Blue of int
    | Red of int
    | Green of int

type Draw = Color list

type GameRecord = { Id: int; Draws: Draw list }
type Game = Game of GameRecord

// parse the `Game xx` part
let getGameIdParser =
    let parser = pstring "Game " >>. manyDigit
    parser

let colorAndNumberParser =
    let inner input =
        let parser = (manyDigit .>> whitespace) .>>. anyOfString [ "blue"; "green"; "red" ]

        let result = run parser input

        match result with
        | Failure msg -> Failure msg
        | Success((number, str), remaining) ->

            match str with
            | "blue" -> Success(Blue number, remaining)
            | "green" -> Success(Green number, remaining)
            | "red" -> Success(Red number, remaining)
            | _ -> Failure(sprintf "Invalid color: %s" str)

    Parser inner

// One draw: 3 green, 4 blue, 1 red
let manyColorsAndNumberParse =
    colorAndNumberParser .>>. many (pstring ", " >>. colorAndNumberParser)
    |>> fun (firstColor, rest) -> firstColor :: rest

let gameOfParseResult ((gameId, firstDraw: Draw), otherDraw: Draw list) =
    Game
        { Id = gameId
          Draws = firstDraw :: otherDraw }

let firstDraw = pstring ": " >>. manyColorsAndNumberParse
let otherDraw = many (pstring "; " >>. manyColorsAndNumberParse)

let lineParser = getGameIdParser .>>. firstDraw .>>. otherDraw |>> gameOfParseResult

let (|SuccessAndEmptyRemaining|_|) =
    function
    | Success(value, "") -> Some value
    | _ -> None

// invalid if any red > 12 ; any green > 13 ; any blue > 14
let isDrawValid (draw: Draw) =
    let colorToIsValid =
        function
        | Blue n -> n <= 14
        | Red n -> n <= 12
        | Green n -> n <= 13 in

    List.map colorToIsValid draw |> List.forall id

let idIfGameValid (game: Game) =
    let (Game g) = game in
    let allDrawsValids = List.map isDrawValid g.Draws |> List.forall id in
    if allDrawsValids then Some g.Id else None

let lines = System.IO.File.ReadAllLines "input.txt"

let getMaxRedAndMaxBlueAndMaxGreen (game: Game) =
    let (Game g) = game

    let maxRed =
        g.Draws
        |> List.collect id
        |> List.map (function
            | Red n -> Some(n)
            | _ -> None)
        |> List.choose id
        |> List.max

    let maxBlue =
        g.Draws
        |> List.collect id
        |> List.map (function
            | Blue n -> Some(n)
            | _ -> None)
        |> List.choose id
        |> List.max

    let maxGreen =
        g.Draws
        |> List.collect id
        |> List.map (function
            | Green n -> Some(n)
            | _ -> None)
        |> List.choose id
        |> List.max

    maxRed * maxBlue * maxGreen


let result =
    lines
    |> Array.map (fun line -> run lineParser line)
    |> Array.choose (function
        | SuccessAndEmptyRemaining value -> Some value
        | a ->
            printf "Invalid line: %A\n" a
            None)

let resultFirstPart = result |> Array.choose idIfGameValid |> Array.sum

let resultSndPart = result |> Array.map getMaxRedAndMaxBlueAndMaxGreen |> Array.sum

printfn "%A" resultFirstPart
printfn "%A" resultSndPart
