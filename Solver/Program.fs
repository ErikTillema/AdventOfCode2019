// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Resources

[<EntryPoint>]
let main argv = 
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
    
    res.GetString("problem1.in")      |> Problem1.solveSilver |> printfn "%d"
    res.GetString("problem1.in")      |> Problem1.solveGold   |> printfn "%d"

    stopWatch.Stop()
    printfn ""
    printfn "Elapsed: %f ms" stopWatch.Elapsed.TotalMilliseconds
    0 // return an integer exit code