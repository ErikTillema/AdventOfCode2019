module Problem6

    open System.Collections.Generic
    open Util

    type Node = { name: string;
                  neighbours: List<Node>;
                  mutable depth: int;
                  mutable dist: int; }

    let parseNodes input = 
        let nodes = Dictionary<string,Node>()
        let getNode name = 
            match nodes.TryGetValue name with
            | true, result -> result
            | _ -> let node = { name = name; neighbours = List<Node>(); depth = -1; dist = -1; }
                   nodes.Add(name, node)
                   node
        let sc = Scanner(input, false)
        for line in sc.Lines do
            match line with
            | Regex "^(?<parent>\S+)\)(?<child>\S+)$" [ parentName ; childName ] ->
                let parent = getNode parentName
                let child = getNode childName
                parent.neighbours.Add(child)
                child.neighbours.Add(parent)
            | _ -> invalidOp "bad line"
        nodes

    let solveSilver input =
        let nodes = parseNodes input
        let root = nodes.["COM"]
        let rec dfs node depth =
            node.depth <- depth
            for n in node.neighbours do
                if n.depth = -1 then
                    dfs n (depth+1)
        dfs root 0
        nodes.Values |> Seq.map (fun node -> node.depth) |> Seq.sum

    let solveGold input = 
        let nodes = parseNodes input
        let start = nodes.["YOU"]
        let endd = nodes.["SAN"]
        
        let q = Queue<Node>()
        start.dist <- 0
        q.Enqueue(start)
        while q.Count > 0 do
            let node = q.Dequeue()
            for n in node.neighbours do
                if n.dist = -1 then
                    n.dist <- node.dist + 1
                    q.Enqueue(n)

        endd.dist - 2
