module Day4

open System.IO
open Microsoft.FSharp.Core

let inputFilePath = Path.Combine(__SOURCE_DIRECTORY__, "Inputs/a.txt")
let lines = File.ReadLines(inputFilePath)

type Direction =
    | Up
    | UpRight
    | Right
    | DownRight
    | Down
    | DownLeft
    | Left
    | UpLeft

let edgeDirectionsFirst: Map<Direction,(int * int)> =
    [
        Up, (-1, 0)
        UpRight, (-1, 1)
        Right, (0, 1)
        DownRight, (1, 1)
        Down, (1, 0)
        DownLeft, (1, -1)
        Left, (0, -1)
        UpLeft, (-1, -1)
    ] |> Map.ofList

let moveForward (position: int * int) (dir: Direction) : int * int =
    let y, x = position
    let dy, dx = edgeDirectionsFirst[dir]
    (y + dy, x + dx)

let splitLines (input: string seq) : Map<int * int, char> =
    input
    |> Seq.mapi (fun yCoord line ->
        line
        |> Seq.mapi (fun xCoord char -> ((yCoord, xCoord), char))
    )
    |> Seq.collect id // flatten
    |> Map.ofSeq

let startPositions (input: Map<int * int, char>) (startLetter: char) : (int * int) seq =
    input
    |> Map.keys
    |> Seq.filter (fun coords -> input[coords] = startLetter)


let rec getSeqInDirection (input: Map<int * int, char>) (startPos: int * int) (dir: Direction) : string =
    match Map.tryFind startPos input with
    | Some charAtPos ->
        let nextCoord = moveForward startPos dir
        string charAtPos + (getSeqInDirection input nextCoord dir)
    | None ->
        ""

let targetFirst = "XMAS"

// misread the assignment (:
// let rec countAllOccurrences (haystack: string) (needle: string) : int =
//     if needle.Length = 0 then
//         1
//     elif haystack.Length = 0 then
//         0
//     else
//         // there is both a part of haystack and part of needle left
//         if needle[0] = haystack[0] then
//             (countAllOccurrences (haystack.Substring(1)) (needle.Substring(1)))
//             + (countAllOccurrences (haystack.Substring(1)) needle)
//         else
//             countAllOccurrences (haystack.Substring(1)) needle

let calculateFirst (input: string seq) : int =
    let mp = input |> splitLines
    let sp = startPositions mp targetFirst[0]
    let strings = sp
                |> Seq.collect (fun sp ->
                    edgeDirectionsFirst
                        |> Map.keys
                        |> Seq.collect (fun dir -> Seq.singleton (getSeqInDirection mp sp dir))
                    )
    strings
    |> Seq.map _.StartsWith(targetFirst)
    |> Seq.filter id
    |> Seq.length


let resultFirst = lines |> calculateFirst

// Part Two

let targetSecond = "MAS"
let edgeDirectionsSecond: Map<Direction,(int * int)> =
    [
        UpRight, (-1, 1)
        DownRight, (1, 1)
        DownLeft, (1, -1)
        UpLeft, (-1, -1)
    ] |> Map.ofList

let moveBackwards (position: int * int) (dir: Direction) : int * int =
    let y, x = position
    let dy, dx = edgeDirectionsFirst[dir]
    (y - dy, x - dx)
let calculateSecond (input: string seq) : int =
    let mp = input |> splitLines
    let sp = startPositions mp targetSecond[1]
    let strings = sp
                |> Seq.map (fun sp ->
                    edgeDirectionsSecond
                        |> Map.keys
                        |> Seq.collect (fun dir ->
                            let newSp = moveBackwards sp dir
                            Seq.singleton (getSeqInDirection mp newSp dir))
                    )
    strings
    |> Seq.map (fun diagonals -> diagonals |> Seq.map _.StartsWith(targetSecond) |> Seq.filter id |> Seq.length |> (fun x -> x = 2))
    |> Seq.filter id
    |> Seq.length
let resultSecond =  lines |> calculateSecond

let run () =
    printfn $"Result first: %d{resultFirst}"
    printfn $"Result second: %d{resultSecond}"
