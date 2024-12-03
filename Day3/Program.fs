module Day3

open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Core

let inputFilePath = Path.Combine(__SOURCE_DIRECTORY__, "Inputs/a.txt")
let lines = File.ReadLines(inputFilePath)

let mulRegex = @"mul\((\d{1,3}),(\d{1,3})\)"

let concatStrings (strings: string seq) : string =
    strings |> Seq.fold (fun accumulator str -> accumulator + str) ""

let splitLineFirst (input: string) : (int * int) seq =
    input
    |> (fun s -> Regex.Matches(s, mulRegex))
    |> Seq.cast<Match>
    |> Seq.map (fun m -> (int m.Groups.[1].Value, int m.Groups.[2].Value))

let calculateFirst (input: string) : int =
    input |> splitLineFirst |> Seq.map (fun (a, b) -> a * b) |> Seq.sum

let resultFirst = lines |> concatStrings |> calculateFirst


// Part Two
let dontDoRegex = @"don't\(\).*?do\(\)"
let trailingDontRegex = @"don't\(\).*"


let splitLineSecond (line: string) =
    line
    |> (fun s -> Regex.Split(s, dontDoRegex))
    |> concatStrings
    |> (fun s -> Regex.Split(s, trailingDontRegex))
    |> concatStrings
    |> calculateFirst

let resultSecond = lines |> concatStrings |> splitLineSecond

let run () =
    printfn $"Result first: %d{resultFirst}"
    printfn $"Result second: %d{resultSecond}"
