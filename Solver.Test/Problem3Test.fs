namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem3
open System.Resources

type Problem3Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83" |> should equal 159
         solveSilver "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" |> should equal 135

    [<Fact>]
    member x.solveGold_works () = 
         solveGold "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83" |> should equal 610
         solveGold "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" |> should equal 410

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem3.in")) |> should equal 5357

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem3.in")) |> should equal 101956

     