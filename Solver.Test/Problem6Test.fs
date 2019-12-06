namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem6
open System.Resources

type Problem6Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L"                  |> should equal 42

    [<Fact>]
    member x.solveGold_works () = 
         solveGold "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN"               |> should equal 4

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem6.in")) |> should equal 270768
        
    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem6.in")) |> should equal 451

     