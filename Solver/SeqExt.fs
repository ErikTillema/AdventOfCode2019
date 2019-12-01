module SeqExt

    open System
    open System.Collections.Generic

    /// Returns a generating function for the sequence.
    /// Use generator by simply calling the function with empty set of parameters ().
    /// Opposite of SeqExt.unfold2: mySequence |> SeqExt.getGenarator |> SeqExt.unfold2 equals mySequence
    let getGenerator (sequence: 'a seq) = 
        let enumerator = sequence.GetEnumerator()
        fun () -> 
            match enumerator.MoveNext() with
            | true -> Some(enumerator.Current)
            | false -> None

    /// Like Seq.unfold, but doesn't require state
    /// This function uses the generator to unfold a sequence, continuing while generator returns Some(element)
    /// and stopping when it returns None.
    /// Opposite of SeqExt.getGenerator: mySequence |> SeqExt.getGenerator |> SeqExt.unfold2 equals mySequence
    let unfold2 generator =
        let g dummyState = 
            match generator() with
            | Some(element) -> Some(element, dummyState)
            | None -> None
        Seq.unfold g 0

    /// Seq.findLast (predicate)
    /// Exactly like List.findBack but it start at the front instead of the back.
    /// Hmm, Seq.findBack already exists... and a Sequence cannot start at the back, can it?
    /// So that would mean that findBack goes forwards, not backwards....
    /// I think that findBack for a sequence works the same way as findBack for a list, and it will loop over the complete sequence.
    /// (just like List.findBack will by the way)
    /// So Array.findBack is more efficient in some cases? We can test that, but I haven't seen an example yet.

    /// returns the first n items of source or less if there are fewer items.
    let takeAtMost n (source: 'a seq) = 
        Seq.truncate n source
        //let enumerator = source.GetEnumerator()
        //let rec takeAtMost' n = 
        //    seq {
        //        if n > 0 && enumerator.MoveNext() then
        //            yield enumerator.Current
        //            yield! takeAtMost' (n-1)
        //    }
        //takeAtMost' n

    ///// The following implementation of the takeAtMost method is dangerous, since the Seq.isEmpty might read off one
    ///// We can prove this with a sequence generator and Seq.unfold and then this implementation.
    ///// It could be fixed with Seq.cache, which adds a cache to the reading of the sequence.
    //let rec takeAtMost n source = 
    //    //let source = Seq.cache source
    //    seq {
    //        if n > 0 && not(Seq.isEmpty source) then
    //            yield Seq.head source
    //            yield! takeAtMost (n-1) (source |> Seq.skip 1)
    //    }
    //[<Fact>]
    //member x.takeAtMost_doesnt_work_with_seq_unfold () = 
    //    let g = seq { 1..20 } |> getGenerator
    //    let s = Seq.unfold (fun s -> match g() with 
    //                                   | Some(v) -> Some(v,0)
    //                                   | None -> None
    //                         0
    //    takeAtMost 4 s |> Seq.toList |> should equal [1..4]  // results in something like [ 2; 6; 12; ... ]

    /// returns whether all items in source are equal
    let allEqual source =
        match Seq.tryHead source with
        | None -> true
        | Some(h) -> source |> Seq.forall ((=) h)
    
    /// returns the Cartesian product of 2 sequences
    let cartesianProduct seq1 seq2 = 
        let array2 = seq2 |> Seq.toArray
        seq1 |> Seq.collect (fun el1 ->
            array2 |> Seq.map (fun el2 -> (el1, el2))
        )

    let rng = new Random()

    /// returns a shuffled version of original (as array)
    let shuffle (original : 'a seq) =
        let res = Seq.toArray original
        let n = res.Length
        for x in 1..n do
            let i = n-x
            let j = rng.Next(i+1)
            let tmp = res.[i]
            res.[i] <- res.[j]
            res.[j] <- tmp
        res

    /// returns all subsets of source (as lists)
    let getSubsets (source: 'a seq) = 
        let source = source |> Seq.toArray
        let rec getSubsets' don acc =
            seq {
                if don = source.Length then
                    yield List.rev acc
                else
                    yield! getSubsets' (don+1) acc
                    yield! getSubsets' (don+1) (source.[don] :: acc)
            }
        getSubsets' 0 []

    /// returns all combinations of source (as arrays)
    let getChooseCombinations n (source: 'a seq) = 
        let source = source |> Seq.toArray
        let sourceCount = source.Length
        let result = Array.zeroCreate n
        let rec getChooseCombinations' don ins =
            seq {
                if ins = n then
                    yield result
                else
                    let remainingItemsAfterThisOne = sourceCount - don - 1
                    // try to put in result
                    if ins < n then
                        result.[ins] <- source.[don]
                        yield! getChooseCombinations' (don+1) (ins+1)

                    // try to leave out of result
                    if ins + remainingItemsAfterThisOne >= n then
                        yield! getChooseCombinations' (don+1) ins
            }
        getChooseCombinations' 0 0

    /// returns all permutations of source (as arrays)
    let getPermutations (source: 'a seq) = 
        let source = source |> Seq.toArray
        let n = source.Length
        let result = Array.zeroCreate n
        let used = Array.create n false
        let rec getPermutations' don =
            seq {
                if don = n then
                    yield result
                else
                    for i in 0..n-1 do
                        if not used.[i] then
                            used.[i] <- true
                            result.[don] <- source.[i]
                            yield! getPermutations' (don+1)
                            used.[i] <- false
            }
        getPermutations' 0

    /// returns all rotations of source (as lists)
    let getRotations (source: 'a seq) = 
        let source = source |> Seq.toList
        let n = source.Length
        seq {
            if n = 0 then
                yield [] // Seq.empty
            else
                for i in 0..n-1 do
                    // the list implementation is faster (2-5 times) if you do something (count over, sum over) with the result lists
                    // the seq implementations is much much faster if you do nothing with the result lists (except count them)
                    // so which to choose... I guess normally you do something with the results, so the List implementation it is.
                    //yield Seq.append (source |> Seq.skip i |> Seq.take (n-i)) (source |> Seq.take i)
                    yield List.append (source |> List.skip i |> List.take (n-i)) (source |> List.take i)
        }
        
    /// returns source sequence in chunks of n as arrays.
    let chunk n (source: 'a seq) = 
        Seq.chunkBySize n source

    /// Returns source sequence split at index n.
    /// Returns the head as a list, because we need to iterate the first n elements anyway (in case head is never iterated for example).
    /// For example, splitAt 3 (seq{1..10}) = ( [1..3] , seq{4..10} )
    /// Throws a InvalidOperationException if n is greater than source.length
    let splitAt n (source: 'a seq) = 
        let generator = source |> getGenerator
        let head = List.init n  (fun _ ->   match generator() with
                                            | Some(v) -> v
                                            | None -> invalidOp "source does not contain n elements"
                                )
        let tail = unfold2 generator
        (head, tail)
