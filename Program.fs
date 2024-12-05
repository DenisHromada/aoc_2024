module AdventOfCode2024.Program

open System.Diagnostics
let stopwatch = Stopwatch.StartNew()

// Day1.run()
// Day2.run()
// Day3.run()
Day4.run()

stopwatch.Stop()
printfn $"Execution time: %f{stopwatch.Elapsed.TotalMilliseconds} ms"
