module Problem2

    open System.Collections.Generic
    open Util

    type State = {  variableValues: int[];
                    mutable position: int }

    let parseState input = 
        let sc = Scanner(input, false, ",")
        { variableValues = sc.Ints |> Array.ofSeq;
          position = 0 }

    let doOperation (state: State) =
        let op = state.variableValues.[state.position]
        let getVal i = // i is 1-based
            let arg = state.variableValues.[state.position+i]
            state.variableValues.[arg]
        let setVal i value = 
            let arg = state.variableValues.[state.position+i]
            state.variableValues.[arg] <- value
        match op with
        | 1 -> setVal 3 ((getVal 1) + (getVal 2))
        | 2 -> setVal 3 ((getVal 1) * (getVal 2))
        | _ -> invalidOp "Unexpected operator"
        state.position <- state.position + 4

    let rec doOperations (state: State) =
        let op = state.variableValues.[state.position]
        match op with
        | 99 -> ()
        | _  -> doOperation state
                doOperations state

    let solveSilver input noun verb =
        try 
            let state = parseState input
            state.variableValues.[1] <- noun
            state.variableValues.[2] <- verb
            doOperations state
            state.variableValues.[0]
        with 
        | :? System.IndexOutOfRangeException -> -1

    let solveGold input = 
        let (noun,verb) = SeqExt.cartesianProduct [0..99] [0..99] |> Seq.find (fun (noun, verb) -> (solveSilver input noun verb) = 19690720)
        100*noun + verb

