open System
// Yeah, it's not union find, cause it's eager. Eat it.
type UnionFindFields = Root of Set<int> | Pointer of int
type UnionFind = Map<int, UnionFindFields>
let tryFind x uf = Map.tryFind x uf |> Option.defaultValue (Root <| Set.singleton x)
let rec findRoot x uf =
    match tryFind x uf with
    | Root _ -> x 
    | Pointer y -> findRoot y uf
let size (Root x) = Set.count x
let setRoot root uf v = Map.add v (Pointer root) uf
let mergeTrees newRoot oldRoot uf = 
    let (Root newRootSet) = tryFind newRoot uf in
    let (Root oldRootSet) = tryFind oldRoot uf in
    let withNewRoot = Map.add newRoot (Root <| Set.union newRootSet oldRootSet) uf in
    oldRootSet |> Set.remove newRoot |> Set.toSeq |> Seq.fold (setRoot newRoot) withNewRoot
let merge x uf y =
    let parentX = findRoot x uf in
    let treeX = tryFind parentX uf in
    let parentY = findRoot y uf in
    let treeY = tryFind parentY uf in
    if (size treeX) >= (size treeY)
    then mergeTrees parentX parentY uf
    else mergeTrees parentY parentX uf
type Pipe = {_from: int; _to: Set<int>}
let parseLine (line: string) =
    let parseInt = Int64.Parse >> int in
    let [|(root: string); (set: string)|] = line.Replace("<->", "|").Split('|') in
    let parsedSet = set.Replace(", ", ",").Split(',') |> Array.map parseInt |> Set.ofArray in
    {_from=parseInt root; _to=parsedSet}
let parseList (list: string) =
    list.Split('\n')
    |> Array.filter (String.IsNullOrEmpty >> not)
    |> Array.map parseLine
let union uf {_from=_from;_to=_to} = Set.toSeq _to |> Seq.fold (merge _from) uf
let double f a b = let y = f a b in (y,y)
let solve1 = parseList >> Seq.fold union Map.empty >> tryFind 0 >> fun (Root v) -> Set.count v
let example = """
0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5
"""
let input = System.IO.File.ReadAllText "Day12\\input.txt"
let isRoot = function
    | Root _ -> true
    | _ -> false
let solve2 = parseList >> Seq.fold union Map.empty >> Map.toSeq >> Seq.map snd >> Seq.filter isRoot >> Seq.length
