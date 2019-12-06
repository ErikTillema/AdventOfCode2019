namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem2
open System.Resources

type Problem2Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver "1,9,10,3,2,3,11,0,99,30,40,50" 9 10 |> should equal 3500
         solveSilver "1,0,0,0,99" 0 0 |> should equal 2
         solveSilver "2,3,0,3,99" 3 0 |> should equal 2
         solveSilver "2,4,4,5,99,0" 4 4 |> should equal 2
         solveSilver "1,1,1,4,99,5,6,0,99" 1 1 |> should equal 30

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem2.in")) 12 2 |> should equal 5110675

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem2.in")) |> should equal 4847

     