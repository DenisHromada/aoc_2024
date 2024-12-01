open System
open System.IO
open Microsoft.FSharp.Core

let inputFilePath = Path.Combine(__SOURCE_DIRECTORY__, "Inputs/a.txt")
let lines = File.ReadLines(inputFilePath)

let splitLine (line: string) : int * int =
    let parts = line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
    if parts.Length = 2 then
        (Int32.Parse(parts.[0]), Int32.Parse(parts.[1]))
    else
        failwithf $"Invalid line format: '%s{line}'"

let linesToLists (lines: string seq) : int list * int list =
    lines
    |> Seq.map splitLine
    |> Seq.fold (fun (listA, listB) (a, b) -> (a :: listA, b :: listB)) ([], [])

let getDistances (a: int list, b: int list) : int =
    let sortedA, sortedB = List.sortDescending a, List.sortDescending b
    List.map2 (fun x y -> abs (x - y)) sortedA sortedB
    |> List.sum


let listA, listB = linesToLists lines
let resultFirst = getDistances (listA, listB)

printfn $"Result first: %d{resultFirst}\n"

// Part Two
let frequencyMap =
    listB
    |> Seq.countBy id
    |> Map.ofSeq

let resultSecond =
    listA
    |> List.map (fun x ->
        let freq = Map.tryFind x frequencyMap |> Option.defaultValue 0
        x * freq
        )
    |> List.sum

printfn $"Result second: %d{resultSecond}\n"
