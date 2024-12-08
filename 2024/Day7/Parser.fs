namespace Day2.Parser

open System

type ParseResult<'a> =
    | Success of 'a
    | Failure of string

type Parser<'T> = Parser of (string -> ParseResult<'T * string>)

module Parser =
    let pchar charToMatch =
        let inner str =
            if String.IsNullOrEmpty(str) then
                Failure "No more input"
            else
                let first = str.[0]

                if str.[0] = charToMatch then
                    let remaining = str.[1..]
                    Success(charToMatch, remaining)
                else
                    Failure(sprintf "Expecting '%c'. Got '%c'" charToMatch first)

        Parser inner

    let anyChar =
        let inner str =
            if String.IsNullOrEmpty(str) then
                Failure "No more input"
            else
                let first = str.[0]
                let remaining = str.[1..]
                Success(first, remaining)

        Parser inner

    let run parser input =
        let (Parser innerFn) = parser
        innerFn input

    let andThen parser1 parser2 =
        let inner input =
            let result1 = run parser1 input

            match result1 with
            | Failure msg -> Failure msg
            | Success(value1, remaining1) ->
                let result2 = run parser2 remaining1

                match result2 with
                | Success(value2, remaining2) -> Success((value1, value2), remaining2)
                | Failure msg -> Failure msg

        Parser inner

    let orElse parser1 parser2 =
        let inner input =
            let result1 = run parser1 input

            match result1 with
            | Success result -> result1
            | Failure err -> run parser2 input

        Parser inner

    let mapP f parser =
        let inner input =
            let result = run parser input

            match result with
            | Success(value, remaining) -> Success(f value, remaining)
            | Failure msg -> Failure msg

        Parser inner

    let returnP a =
        let inner input = Success(a, input)
        Parser inner


    let (.>>.) = andThen
    let (<|>) = orElse
    let (<!>) = mapP
    let (|>>) x f = mapP f x


    let choice listOfParsers = List.reduce (<|>) listOfParsers

    let anyOf listOfChars = listOfChars |> List.map pchar |> choice


    let applyP fP xP =
        (fP .>>. xP) |> mapP (fun (f, x) -> f x)

    let (<*>) = applyP

    let lift2 f xP yP = (returnP f) <*> xP <*> yP


    let startsWith (str: string) (prefix: string) = str.StartsWith(prefix)

    let startsWithP = lift2 startsWith

    let rec sequence parserList =
        let cons head tail = head :: tail
        let consP = lift2 cons

        match parserList with
        | [] -> returnP []
        | head :: tail -> consP head (sequence tail)

    let rec parseZeroOrMore parser input =
        let result = run parser input

        match result with
        | Failure _ -> ([], input)
        | Success(value, remaining) ->
            let (values, remaining) = parseZeroOrMore parser remaining
            (value :: values, remaining)

    let many parser =
        let inner input =
            let (result, remaining) = parseZeroOrMore parser input
            Success(result, remaining)

        Parser inner

    let many1 parser =
        let inner input =
            let firstResult = run parser input

            match firstResult with
            | Failure msg -> Failure msg
            | Success(value, remaining) ->
                let (result, remaining) = parseZeroOrMore parser remaining
                Success(value :: result, remaining)

        Parser inner

    let whitespaceChar = anyOf [ ' '; '\t'; '\n' ]
    let whitespace = many whitespaceChar
    let digit = anyOf [ '0' .. '9' ]

    let pstring (str: string) =
        let inner input =
            let parserSeq = Seq.toList str |> List.map pchar
            let combinedParser = sequence parserSeq
            let result = run combinedParser input

            match result with
            | Success(value, remaining) -> Success(str, remaining)
            | Failure msg -> Failure msg

        Parser inner

    let anyOfString listOfString =
        listOfString |> List.map pstring |> choice

    let charArrayToInt = Seq.toArray >> String >> bigint.Parse

    let manyDigit = many1 digit |>> charArrayToInt

    let rec skipManyTill skipParser endParser =
        Parser(fun input ->
            match run endParser input with
            | Success(result, rest) -> Success(result, rest) // End parser succeeded
            | Failure _ ->
                // End parser failed; try applying the skip parser
                match run skipParser input with
                | Success(_, rest) ->
                    // Skip parser succeeded; recurse with the remaining input
                    run (skipManyTill skipParser endParser) rest
                | Failure msg -> Failure msg) // Skip parser failed

    let (.>>) p1 p2 =
        // create a pair
        p1 .>>. p2
        // then only keep the first value
        |> mapP (fun (a, b) -> a)

    /// Keep only the result of the right side parser
    let (>>.) p1 p2 =
        // create a pair
        p1 .>>. p2
        // then only keep the second value
        |> mapP (fun (a, b) -> b)
