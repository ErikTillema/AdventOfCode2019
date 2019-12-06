module Problem5

    open System.Collections.Generic
    open Util

    type State = {  variableValues: int[];
                    mutable position: int;
                    mutable output: int list }

    let parseState input = 
        let sc = Scanner(input, false, ",")
        { variableValues = sc.Ints |> Array.ofSeq;
          position = 0;
          output = [] }

    let doOperation (state: State) fixedInput =
        let getInput() = fixedInput
        let output a = state.output <- a :: state.output
        let opAndModes = state.variableValues.[state.position]
        let op = opAndModes % 100
        let modes = opAndModes / 100
        let getMode i = let mutable m = modes
                        for _ in 0..i-2 do m <- m/10
                        m % 10
        let getVal i = // i is 1-based
            let arg = state.variableValues.[state.position+i]
            match getMode i with 
            | 0 -> state.variableValues.[arg]
            | 1 -> arg
            | _ -> invalidOp "invalid mode"
        let setVal i value = 
            let arg = state.variableValues.[state.position+i]
            match getMode i with 
            | 0 -> state.variableValues.[arg] <- value
            | 1 -> invalidOp "invalid mode for setting value"
            | _ -> invalidOp "invalid mode"
        match op with
            | 1 -> setVal 3 ((getVal 1) + (getVal 2))
                   state.position <- state.position + 4
            | 2 -> setVal 3 ((getVal 1) * (getVal 2))
                   state.position <- state.position + 4
            | 3 -> setVal 1 (getInput())
                   state.position <- state.position + 2
            | 4 -> getVal 1 |> output
                   state.position <- state.position + 2
            | 5 -> if (getVal 1) <> 0 then state.position <- getVal 2
                   else                    state.position <- state.position + 3
            | 6 -> if (getVal 1) = 0  then state.position <- getVal 2
                   else                    state.position <- state.position + 3
            | 7 -> setVal 3 (if (getVal 1) < (getVal 2) then 1 else 0)
                   state.position <- state.position + 4
            | 8 -> setVal 3 (if (getVal 1) = (getVal 2) then 1 else 0)
                   state.position <- state.position + 4
            | _ -> invalidOp "Unexpected operator"

    let rec doOperations (state: State) input =
        let op = state.variableValues.[state.position]
        match op with
        | 99 -> ()
        | _  -> doOperation state input
                doOperations state input

    let solveSilver input =
        let state = parseState input
        doOperations state 1
        state.output.Head

    let solveGold input = 
        let state = parseState input
        doOperations state 5
        state.output.Head
