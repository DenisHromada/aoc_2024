module Day2

open System
open System.IO
open Microsoft.FSharp.Core


let inputFilePath = Path.Combine(__SOURCE_DIRECTORY__, "Inputs/a.txt")
let lines = File.ReadLines(inputFilePath)

let splitLine (line: string) : int seq =
    line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map Int32.Parse

type Delta =
    | Positive
    | Negative
    | Invalid

let distinctDeltasSafetyCondition (distinctDeltas: Delta seq) : bool =
    not (Seq.length distinctDeltas > 1 || Seq.contains Invalid distinctDeltas)
let isSafe (line: int seq) : bool =
    line
    |> Seq.pairwise
    |> Seq.map (fun (a, b) ->
        if a = b then
            Invalid
        elif abs (a - b) <= 3 then
            if a - b > 0 then Positive else Negative
        else
            Invalid)
    |> Seq.distinct
    |> distinctDeltasSafetyCondition



let resultFirst =
    lines |> Seq.map splitLine |> Seq.map isSafe |> Seq.filter id |> Seq.length


// Part Two
let omitNthElement (n: int) (line: int seq) : int seq =
    line
    |> Seq.indexed
    |> Seq.filter (fun (i, _) -> i <> n)
    |> Seq.map snd

let omitOneVariants (line: int seq) : int seq seq =
    line
    |> Seq.mapi (fun i _ -> omitNthElement i line)

let isSafeDampened (line: int seq) : bool =
    if isSafe line then isSafe line
    else
        line
        |> omitOneVariants
        |> Seq.exists isSafe

let resultSecond =
    lines |> Seq.map splitLine |> Seq.map isSafeDampened |> Seq.filter id |> Seq.length

let run () =
    printfn $"Result first: %d{resultFirst}\n"
    printfn $"Result second: %d{resultSecond}\n"
