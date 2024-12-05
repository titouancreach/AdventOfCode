let input =
    [| "47|53"
       "97|13"
       "97|61"
       "97|47"
       "75|29"
       "61|13"
       "75|53"
       "29|13"
       "97|29"
       "53|29"
       "61|53"
       "97|53"
       "61|29"
       "47|13"
       "75|47"
       "97|75"
       "47|61"
       "75|61"
       "47|29"
       "75|13"
       "53|13"
       ""
       "75,47,61,53,29"
       "97,61,53,29,13"
       "75,29,13"
       "75,97,47,61,53"
       "61,13,29"
       "97,13,75,29,47" |]

let reelInput = System.IO.File.ReadAllLines "input.txt"

type PageBefore = int
type PageAfter = int
type Rule = Rule of (PageBefore * PageAfter)

type PagePrinted = PagePrinted of int array
type PagesPrinted = PagePrinted array

type Rules = Rule array

let parseRule (input: string) : Rule =
    let split = input.Split('|')
    Rule(int split[0], int split[1])

let parsePagePrinted (input: string) : PagePrinted =
    let split = input.Split(',')
    PagePrinted(Array.map int split)

// Splits an array at a given index, removing the pivot element
let splitAt (index: int) (array: 'a array) : ('a array * 'a array) =
    Array.splitAt index array |> fun (a, b) -> (a, b.[1..])

let parse input =
    let splitPoint = Array.findIndex ((=) "") input
    let (a, b) = splitAt splitPoint input

    let rules: Rules = Array.map parseRule a
    let pagesPrinted: PagesPrinted = Array.map parsePagePrinted b

    (rules, pagesPrinted)


// Rules are broken if only one of the rule is broken.
// Here, checking if a rule is broken, is checking if the head should be after a page, but we found the page in tail
let isRuleBroken (Rule(before, after): Rule) (head: int) (tail: int list) =
    if after = head then // we need to check the tail
        // printfn "Checking if %d is in %A" before tail
        let broken = Array.contains before (Array.ofList tail)
        // check if a before is in tail

        broken
    else
        false

let isPagePrintedBroken (rules: Rules) =
    function
    | currPage :: restPage -> Array.exists (fun rule -> isRuleBroken rule currPage restPage) rules
    | _ -> false

let rec atLeastOnePageBroken (rules: Rules) (pages: int list) =
    match pages with
    | [] -> false
    | head :: remainingPages ->
        isPagePrintedBroken rules (head :: remainingPages)
        || atLeastOnePageBroken rules remainingPages

let result =
    let (rules, pagesPrinted) = parse reelInput

    let pagesNotBroken =
        Array.filter (fun (PagePrinted page) -> not (atLeastOnePageBroken rules (page |> Array.toList))) pagesPrinted

    let sum =
        Array.sumBy
            (fun (PagePrinted page) ->
                let middle = Array.length page / 2
                page[middle])
            pagesNotBroken

    sum

let comparer (rules: Rules) a b =
    let found =
        Array.tryFind (fun (Rule(before, after)) -> before = a && after = b) rules

    match found with
    | Some _ -> -1
    | None -> 1



let result2 =
    let (rules, pagesPrinted) = parse reelInput

    let pagesBroken =
        Array.filter (fun (PagePrinted page) -> (atLeastOnePageBroken rules (page |> Array.toList))) pagesPrinted

    let sum =
        pagesBroken
        |> Array.map (fun (PagePrinted(page)) -> PagePrinted(Array.sortWith (comparer rules) page))
        |> Array.sumBy (fun (PagePrinted(page)) ->
            let middle = Array.length page / 2
            page[middle])

    sum


printfn "%A" <| result
printfn "%A" <| result2
