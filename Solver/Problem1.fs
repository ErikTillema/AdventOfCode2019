module Problem1

    open System.Collections.Generic
    open Util

    let parseNumbers input = 
        let sc = Scanner(input, false)
        sc.Ints |> Array.ofSeq

    let rec calcFuel = 
        let cache = Dictionary<int,int>()
        (fun mass -> 
                if not(cache.ContainsKey(mass)) then 
                    let fuel = (mass/3) - 2
                    let result = if fuel <= 0 then 0 else fuel + calcFuel(fuel)
                    cache.[mass] <- result
                cache.[mass]
        )
    
    let solveSilver input = 
        let numbers = parseNumbers input
        numbers |> Seq.map (fun mass -> (mass/3) - 2) |> Seq.sum

    let solveGold input = 
        let numbers = parseNumbers input
        numbers |> Seq.map calcFuel |> Seq.sum
