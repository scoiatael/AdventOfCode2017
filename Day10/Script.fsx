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
let parseList (list: string) = list.Split(',') |> Array.map (Int64.Parse >> int)
let simulate1 list = parseList >> Seq.mapFold advance {cursor=0; list=list; skipSize=0}
let solve1 = simulate1 ([0..255]|> Array.ofList) >> snd >> fun {list=list} -> Array.reduce (*) list.[0..1]
let KnotHashSuffix = [|17; 31; 73; 47; 23|]
let parseAscii (list: string) = list.ToCharArray() |> Array.map int
let toDenseHash = Array.indexed >> Array.groupBy (fun (a,_) -> a / 16) >> Array.map (fun (_, ls) -> Array.map snd ls |> Array.reduce (^^^))
let createList = parseAscii >> (fun l -> Array.append l KnotHashSuffix)
let copy n ls = Array.concat ([0..n-1] |> List.map (fun _ -> ls))
let toHex = Array.fold (fun st i -> sprintf "%s%02x" st i) ""
let solve2 = createList >> copy 64 >> Seq.mapFold advance {cursor=0; list=([0..255] |> Array.ofList); skipSize=0} >> snd >> (fun {list=list} -> list) >> toDenseHash >> toHex