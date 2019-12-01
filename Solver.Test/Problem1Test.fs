namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem1
open System.Resources

type Problem1Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver "12" |> should equal 2
         solveSilver "14" |> should equal 2
         solveSilver "1969" |> should equal 654
         solveSilver "100756" |> should equal 33583

    [<Fact>]
    member x.solveGold_works () = 
         solveGold "14" |> should equal 2
         solveGold "1969" |> should equal 966
         solveGold "100756" |> should equal 50346

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem1.in")) |> should equal 3235550

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem1.in")) |> should equal 4850462

     