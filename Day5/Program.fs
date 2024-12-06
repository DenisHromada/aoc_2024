module Day5

open System.IO
open Microsoft.FSharp.Core

let inputFilePath = Path.Combine(__SOURCE_DIRECTORY__, "Inputs/a.txt")
let lines = File.ReadLines(inputFilePath)

let splitLines (input: string seq) : Map<int, Set<int>> * int seq seq =
    let precedenceStrings, orderingStrings =
        input
        |> Seq.fold
            (fun (precedences, orderings) line ->
                if line.Contains("|") then
                    (Seq.append precedences (seq { line }), orderings)
                elif line.Contains(",") then
                    (precedences, Seq.append orderings (seq { line }))
                else
                    (precedences, orderings))
            (Seq.empty, Seq.empty)

    let precedenceMap: Map<int, Set<int>> =
        precedenceStrings
        |> Seq.fold
            (fun precedenceMap line ->
                let before, after = line.Split("|") |> (fun split -> int split[0], int split[1])

                match precedenceMap |> Map.tryFind before with
                | Some(existingSet) -> precedenceMap |> Map.add before (existingSet.Add after)
                | None -> precedenceMap |> Map.add before (Set.empty.Add after))
            Map.empty

    let manuals =
        orderingStrings |> Seq.map (fun line -> line.Split(",") |> Seq.map int)

    precedenceMap, manuals

let rec prefixValid (precedenceMap: Map<int, Set<int>>) (inputSeq: int seq) (prefixSet: Set<int>) : bool =
    let head, newInputSeq = (Seq.head inputSeq), (Seq.tail inputSeq)
    let newPrefix = prefixSet.Add head

    match Map.tryFind head precedenceMap with
    | Some(existingSet) ->
        if Set.count (Set.intersect existingSet prefixSet) <> 0 then
            false
        else if Seq.length newInputSeq = 0 then
            true
        else
            prefixValid precedenceMap newInputSeq newPrefix
    | None ->
        if Seq.length newInputSeq = 0 then
            true
        else
            prefixValid precedenceMap newInputSeq newPrefix

let calculateFirst (input: string seq) =
    let precedenceMap, manuals = input |> splitLines

    manuals
    |> Seq.filter (fun line -> (prefixValid precedenceMap line Set.empty)) // valid manuals
    // |> Seq.map (fun line ->
    //     (printfn $"Valid manual: %A{Seq.toList line}"
    //      line))
    |> Seq.map (fun line ->
        let middleIndex = (Seq.length line / 2)
        Seq.item middleIndex line)
    |> Seq.sum

let resultFirst = lines |> calculateFirst

// Part Two
let rec findIncorrectPositions
    (precedenceMap: Map<int, Set<int>>)
    (inputSeq: int seq)
    (curPos: int)
    (prefixMap: Map<int, int>)
    (prefixSet: Set<int>)
    : int * int =
    let head, newInputSeq = (Seq.head inputSeq), (Seq.tail inputSeq)
    let newPrefixMap = prefixMap.Add(head, curPos)
    let newPrefixSet = prefixSet.Add head
    let newCurPos = curPos + 1

    match Map.tryFind head precedenceMap with
    | Some(existingSet) ->
        let conflicts = Set.intersect existingSet prefixSet

        if Set.count conflicts <> 0 then
            prefixMap[conflicts.MinimumElement], curPos // it is assumed there is only one element in the intersection
        else if Seq.length newInputSeq = 0 then
            -1, -1
        else
            findIncorrectPositions precedenceMap newInputSeq newCurPos newPrefixMap newPrefixSet
    | None ->
        if Seq.length newInputSeq = 0 then
            -1, -1
        else
            findIncorrectPositions precedenceMap newInputSeq newCurPos newPrefixMap newPrefixSet


let calculateSecond (input: string seq) : int =
    let precedenceMap, manuals = input |> splitLines

    manuals
    |> Seq.filter (fun line -> not (prefixValid precedenceMap line Set.empty)) // invalid manuals
    // |> Seq.map (fun line ->
    //     (printfn $"Invalid manual: %A{Seq.toList line}"
    //      line))
    |> Seq.map (fun line ->
        let mutable newLineList: int list = line |> Seq.toList
        let mutable continueSwapping = true

        while continueSwapping do
            let a, b =
                findIncorrectPositions precedenceMap (List.toSeq newLineList) 0 Map.empty Set.empty

            if a = -1 && b = -1 then
                continueSwapping <- false
            else
                let aVal, bVal = newLineList[a], newLineList[b]

                newLineList <-
                    newLineList
                    |> List.mapi (fun i value ->
                        if i = a then bVal
                        elif i = b then aVal
                        else value)

        List.toSeq newLineList)
    |> Seq.map (fun line ->
        let middleIndex = (Seq.length line / 2)
        Seq.item middleIndex line)
    |> Seq.sum

let resultSecond = lines |> calculateSecond

let run () =
    printfn $"Result first: %d{resultFirst}"
    printfn $"Result second: %d{resultSecond}"
