open System
open System.Linq.Expressions
let sumMap map (k, v) = Map.add k (v + (Map.tryFind k map |> Option.defaultValue 0)) map
let generateIncrements from howMany length = 
    Seq.unfold (fun (left, ind) -> if left = 0 then None else Some ((ind, 1),(left - 1, (ind + 1) % length))) (howMany, from)
    |> Seq.fold sumMap Map.empty
    |> Map.toSeq
let generate (config, history) = 
    let length = Map.toSeq config |> Seq.length in
    let maxValue = Map.toSeq config |>  Seq.map snd |> Seq.reduce max in
    let maxValueIndex = Map.findKey (fun _ v -> v = maxValue) config in
    //printf "%A: %A - %A\n" maxValue maxValueIndex length;
    let increments = generateIncrements ((maxValueIndex + 1) % length) maxValue length in
    //printf "%A\n" (Array.ofSeq increments);
    let newConfiguration =  Seq.fold sumMap (Map.add maxValueIndex 0 config) increments in
    if Set.contains newConfiguration history    
    then None
    else
        Some
            ( newConfiguration
            , (newConfiguration, Set.add newConfiguration history))
let parse(line: string) = 
            line.Split(' ') 
            |> Array.map (Int64.Parse >> int)
let startConfiguration input= parse input |> Seq.indexed |> Map.ofSeq
let solve1 input = 
    Seq.unfold generate (startConfiguration input, Set.empty) |> Seq.length |> (+) 1
let generate2 (config, history) = 
    let length = Map.toSeq config |> Seq.length in
    let maxValue = Map.toSeq config |>  Seq.map snd |> Seq.reduce max in
    let maxValueIndex = Map.findKey (fun _ v -> v = maxValue) config in
    //printf "%A: %A - %A\n" maxValue maxValueIndex length;
    let increments = generateIncrements ((maxValueIndex + 1) % length) maxValue length in
    //printf "%A\n" (Array.ofSeq increments);
    let newConfiguration =  Seq.fold sumMap (Map.add maxValueIndex 0 config) increments in
    history 
    |> Option.map (fun hist ->
        let newState = 
            if Set.contains newConfiguration hist    
            then
                (newConfiguration, None)
            else
                ( newConfiguration
                , Some <| Set.add newConfiguration hist)
        in (newConfiguration, newState)
    )
let solve2 input =
    let startConfig = startConfiguration input in
    let solution = Seq.readonly <| Seq.unfold generate2 (startConfig, Some Set.empty) in
    let circular = Seq.last solution in
    let circIndex = Seq.findIndex ((=) circular) solution in
    (Seq.length solution) - (circIndex+1)