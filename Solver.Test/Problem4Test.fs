namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem4
open System.Resources

type Problem4Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver 111111 111111 |> should equal 1
         solveSilver 111111 111113 |> should equal 3
         solveSilver 123456 123456 |> should equal 0

    [<Fact>]
    member x.solveGold_works () = 
         solveGold 111111 111111 |> should equal 0
         solveGold 111111 111122 |> should equal 1

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver 357253 892942 |> should equal 530

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold 357253 892942 |> should equal 324

     