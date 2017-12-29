module Day14
open System
type Knot = {cursor: int; list: int array; skipSize: int}
let move len (list: 'a array) = Array.concat [|list.[len..]; list.[0..len-1]|]
let tie len (list: 'a array) = Array.concat [|Array.rev list.[0..len-1]; list.[len..]|]
let advance {cursor=cursor; list=list; skipSize=skipSize} len =
    let newList = list |> move cursor |> tie len |> move (Array.length list - cursor) in
    let newCursor = (cursor + skipSize + len) % Array.length list in
    (   newList,
        { cursor=newCursor
        ; list=newList
        ; skipSize=skipSize+1 } )
let KnotHashSuffix = [|17; 31; 73; 47; 23|]
let parseAscii (list: string) = list.ToCharArray() |> Array.map int
let toDenseHash = Array.indexed >> Array.groupBy (fun (a,_) -> a / 16) >> Array.map (fun (_, ls) -> Array.map snd ls |> Array.reduce (^^^))
let createList = parseAscii >> (fun l -> Array.append l KnotHashSuffix)
let copy n ls = Array.concat ([0..n-1] |> List.map (fun _ -> ls))
let calculateKnotHash = createList >> copy 64 >> Seq.mapFold advance {cursor=0; list=([0..255] |> Array.ofList); skipSize=0} >> snd >> (fun {list=list} -> list) >> toDenseHash
let rec toBitArr i =
    let powi a b = (float a)**(float b) |> int in
    [0..7] |> List.map (fun exp -> (i / (powi 2 exp)) % 2) |> List.rev
let countOnes = toBitArr >> List.filter ((=) 1) >> List.length
let countOnesInHash = calculateKnotHash >> Array.map countOnes >> Array.reduce (+)
let generateHashInput prefix = [0..127] |> List.map (fun i -> sprintf "%s-%i" prefix i)
let solve1 = generateHashInput >> List.map countOnesInHash >> List.reduce (+)
let example = "flqrgnkx"
let input = "uugsqrei"
let MaxIndex = 128 * 16
let intOfPoint (x, y) = x * MaxIndex + y
let pointOfInt i = (i / MaxIndex, i % MaxIndex)
let neighbours =
    pointOfInt >> fun (x, y) ->
    [x-1,y; x+1, y; x, y-1; x, y+1]
    |> List.map intOfPoint

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
let isRoot = function
    | Root _ -> true
    | _ -> false

let joinAreas areas point map =
    areas |> List.fold (fun uf area -> merge area uf point) map

let joinNeighbours (map: UnionFind) (point: int) =
    let neighboursAreas =
        (neighbours point
        |> List.filter (fun p -> Map.containsKey p map)) in
    let newMap = Map.add point (Set.singleton point |> Root) map
    if List.isEmpty neighboursAreas
    then
        newMap
    else
        joinAreas neighboursAreas point newMap


let solve2 =
    generateHashInput
    >> List.map (calculateKnotHash >> Array.map (toBitArr >> Array.ofList) >> Array.concat)
    >> Array.ofList
    >> Array.map Array.indexed
    >> Array.indexed
    >> Array.map (fun (y, arr) -> Array.map (fun (x, v) -> (v, x, y)) arr)
    >> Array.concat
    >> Array.filter (fun (v, _, _) ->  v = 1)
    >> Array.map (fun (_, x, y) -> intOfPoint (x, y))
    >> Array.fold joinNeighbours Map.empty
    >> Map.toSeq
    >> Seq.filter (snd >> isRoot)
    >> Seq.length
