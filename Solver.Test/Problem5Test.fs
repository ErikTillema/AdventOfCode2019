namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem5
open System.Resources

type Problem5Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver "3,0,4,0,99" |> should equal 1

    [<Fact>]
    member x.solveGold_works () = 
         solveGold "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" |> should equal 999

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem5.in")) |> should equal 13285749

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem5.in")) |> should equal 5000972

     