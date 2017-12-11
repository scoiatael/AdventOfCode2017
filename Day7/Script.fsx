open System
open System.Windows.Forms
let parseFrom (from: string) =
    let bySp = from.Split ' ' in
    (bySp.[0], bySp.[1].Replace('(', ' ').Replace(')', ' ') |> Int64.Parse |> int)
let parseTo (t: string) = t.Split ',' |> Array.map (fun str -> str.Trim())
let parse(lines: string) =
    lines.Split '\n'
    |> Array.map (fun line ->
            let byArr = line.Split '-' in
            if 1 = Array.length byArr
            then (parseFrom byArr.[0], [] |> Array.ofList)
            else (parseFrom byArr.[0], parseTo byArr.[1].[1..])
        )
let solve1 input = 
    let withChildren =
        parse input 
            |> Seq.ofArray 
            |> Seq.filter (fun (_, child) -> 0 <> Array.length child) in
    let children = Seq.map snd withChildren |> Seq.fold (fun st mem -> Set.union (Set.ofArray mem) st) Set.empty in
    let probable = Seq.map (fst >> fst) withChildren in
    (Set.difference (Set.ofSeq probable) children)
    |> Set.toSeq |> Seq.head

type Tree = Leaf of L | Node of N
and L = {name: string; weight: int}
and N = {node: L; children: Tree list}
let rec makeGraph proto root =
    let (leaf, children) = Map.find root proto in
    if 0 = Array.length children
    then Leaf leaf
    else Node {node=leaf; children = Array.map (makeGraph proto) children |> List.ofArray}
let rec weight = function
    | Leaf l -> l.weight
    | Node n -> 
        let childrenWeights = List.map weight n.children |> List.reduce (+) in
        n.node.weight + childrenWeights
let nodeWeight = function
    | Leaf l -> l.weight
    | Node n -> n.node.weight
let nodeName = function
    | Leaf l -> l.name
    | Node n -> n.node.name
let pickMajority ls = List.groupBy id ls |> List.filter (fun (_, vals) -> 1 <> List.length vals) |> List.head |> fst
let pickWrongOne (ls: Tree list) weights good = 
    let ind = List.findIndex ((<>) good) weights in
    (nodeWeight  ls.[ind]) + good - (weight ls.[ind])
let rec checkNode n = 
    let children = (List.map check n.children |> List.filter Option.isSome) in
        match List.tryHead children |> Option.bind id with
        | Some s -> Some s
        | None -> (let childWeights = List.map weight n.children in
            if (List.reduce min childWeights) <> (List.reduce max childWeights)
            then pickMajority childWeights |> pickWrongOne n.children childWeights |> Some
            else None)
and check = function
    | Leaf _ -> None
    | Node n -> checkNode n
let solve2 input = 
    let protoGraph = parse input |> Seq.map (fun ((node, weight), children) -> (node, ({name=node; weight=weight}, children))) |> Map.ofSeq in
    let root = solve1 input in
    let graph = makeGraph protoGraph root in
    check graph |> Option.get