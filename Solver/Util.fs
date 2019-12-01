module Util

    open System
    open System.IO
    open System.Linq
    open System.Text.RegularExpressions
    open SeqExt
    open System.Numerics

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    type Scanner(filepath: string, ?isFile0: bool, ?separators0: string) = 

        let isFile = defaultArg isFile0 true
        let separators = defaultArg separators0 " \t\r\n"
        let mutable atEndOfFile = false
        let reader = 
            if isFile then new StreamReader(filepath) :> TextReader // @@@ using statement?. Make scanner IDisposable?
            else new StringReader(filepath) :> TextReader
    
        let rec next() = 
            let rec nextTokenOrEOF acc = 
                let i = reader.Read()
                if i = -1 then
                    atEndOfFile <- true
                    List.rev acc
                else
                    let c = char i
                    if separators.Contains(c) then List.rev acc
                    else nextTokenOrEOF (c::acc)
            match atEndOfFile with 
            | true -> None
            | false ->
                let chars = nextTokenOrEOF [] |> List.toArray
                if chars.Length > 0 then 
                    Some(chars |> String)
                else 
                    next()

        let nextInt() = next() |> Option.map int
        let nextLong() = next() |> Option.map int64
        let nextBigInteger() = next() |> Option.map (fun s -> BigInteger.Parse(s))
        let nextDouble() = next() |> Option.map double

        let rec lines = 
            seq {
                let line = reader.ReadLine()
                if line <> null then 
                    yield line
                    yield! lines
            }

        let tokens = unfold2 next
        // equivalent of:
        //let rec tokens =
        //    seq {
        //        match next() with
        //        | None -> ()
        //        | Some(token) ->
        //            yield token
        //            yield! tokens
        //    }

        let ints = unfold2 nextInt
        let longs = unfold2 nextLong
        let bigIntegers = unfold2 nextBigInteger
        let doubles = unfold2 nextDouble

        member x.Next() = next()
        member x.NextInt() = nextInt()
        member x.NextLong() = nextLong()
        member x.NextBigInteger() = nextBigInteger()
        member x.NextDouble() = nextDouble()
        member x.Lines = lines
        member x.Tokens = tokens
        member x.Ints = ints
        member x.Longs = longs
        member x.BigIntegers = bigIntegers
        member x.Doubles = doubles
