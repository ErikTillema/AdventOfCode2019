module Problem3

    open System.Collections.Generic
    open Util

    type Line = | HorizontalLine of int*int*int*int // y, x1, x2, steps taken before start of line
                | VerticalLine   of int*int*int*int // x, y1, y1, steps taken before start of line

    let parseLines s =
        let sc = Scanner(s, false, ",")
        let mutable pos = (0,0)
        let mutable steps = 0
        seq {
            let instructions = sc.Tokens |> Seq.map (fun token -> (token.[0], int(token.Substring(1))))
            for (direction,length) in instructions do
                let (x,y) = pos
                match direction with
                | 'U' -> yield VerticalLine(x, y, y+length, steps)
                         pos <- (x, y+length)
                | 'D' -> yield VerticalLine(x, y, y-length, steps)
                         pos <- (x, y-length)
                | 'R' -> yield HorizontalLine(y, x, x+length, steps)
                         pos <- (x+length, y)
                | 'L' -> yield HorizontalLine(y, x, x-length, steps)
                         pos <- (x-length, y)
                | _ -> invalidOp ""
                steps <- steps + length
        }

    let parseLineSets input =
        let sc = Scanner(input, false)
        let inputLines = sc.Lines |> Seq.toArray
        (parseLines inputLines.[0], parseLines inputLines.[1])

    let getCrossingPoint line1 line2 =
        match line1, line2 with
        | HorizontalLine(_,_,_,_), HorizontalLine(_,_,_,_)     -> None
        | VerticalLine(_,_,_,_), VerticalLine(_,_,_,_)         -> None
        | HorizontalLine(y,x1,x2,s1), VerticalLine(x,y1,y2,s2) 
        | VerticalLine(x,y1,y2,s1), HorizontalLine(y,x1,x2,s2) -> if (min y1 y2) <= y && y <= (max y1 y2) && (min x1 x2) <= x && x <= (max x1 x2) then
                                                                      Some(x, y, s1 + s2 + abs(x-x1) + abs(y-y1)) 
                                                                  else None

    let solveSilver input =
        let lines1, lines2 = parseLineSets input
        SeqExt.cartesianProduct lines1 lines2 |> Seq.map (fun (l1,l2) -> getCrossingPoint l1 l2)
                                              |> Seq.filter Option.isSome
                                              |> Seq.map Option.get
                                              |> Seq.filter (fun (x,y,_) -> (x,y) <> (0,0))
                                              |> Seq.map (fun (x,y,_) -> abs(x) + abs(y))
                                              |> Seq.min

    let solveGold input =
        let lines1, lines2 = parseLineSets input
        SeqExt.cartesianProduct lines1 lines2 |> Seq.map (fun (l1,l2) -> getCrossingPoint l1 l2)
                                              |> Seq.filter Option.isSome
                                              |> Seq.map Option.get
                                              |> Seq.filter (fun (x,y,_) -> (x,y) <> (0,0))
                                              |> Seq.map (fun (_,_,steps) -> steps)
                                              |> Seq.min
