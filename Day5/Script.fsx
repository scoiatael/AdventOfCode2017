open System
let parse(word: string) = 
    word.Split('\n') 
    |> Array.map (Int64.Parse >> int)
let prepareJumpMap map jumps = 
    jumps
    |> Seq.indexed
    |> Seq.fold (fun m (ind, v) -> Map.add ind v m) map
let nextStep f (i, t) = 
    Map.tryFind i t 
    |> Option.map
        (fun v -> 
            (i, (i+v, Map.add i (f v) t)))
let jumpAround ind f tab = 
    Seq.unfold (nextStep f)
        (ind, tab)
let solve1(word: string) = word |> parse |> prepareJumpMap Map.empty |> jumpAround 0 (fun v -> v+1) |> Seq.length
let decIfGt3 v = if v >= 3 then v - 1 else v + 1
let solve2(word: string) = word |> parse |> prepareJumpMap Map.empty |> jumpAround 0 decIfGt3 |> Seq.length