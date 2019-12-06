module Problem4

    open System.Collections.Generic
    open Util

    let parseNumbers input = 
        let sc = Scanner(input, false)
        sc.Ints |> Array.ofSeq

    let solveSilver min max = 
        let mutable cnt = 0
        let rec solve password don minDigit lastDigit containsDouble = 
            if don = 6 then
                if min <= password && password <= max && containsDouble then 
                    cnt <- cnt + 1
            else
                for d in minDigit..9 do
                    solve (10*password + d) (don+1) d d (containsDouble || (d=lastDigit))

        solve 0 0 0 -1 false
        cnt

    let solveGold min max = 
        let mutable cnt = 0
        let rec solve password don minDigit lastDigit containsDouble lengthLastMatchingGroup = 
            if don = 6 then
                if min <= password && password <= max && (containsDouble || lengthLastMatchingGroup=2) then 
                    cnt <- cnt + 1
            else
                for d in minDigit..9 do
                    solve (10*password + d) (don+1) d d (containsDouble || (d <> lastDigit && lengthLastMatchingGroup = 2)) 
                                                        (if d = lastDigit then lengthLastMatchingGroup + 1 else 1)

        solve 0 0 0 -1 false 0
        cnt
