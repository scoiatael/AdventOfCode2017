module Day16
open System

type Dancers = char array

let simpleDancers : Dancers = "abcde".ToCharArray()
let inputDancers : Dancers = "abcdefghijklmnop".ToCharArray()

let spin n (dancers : Dancers) =
    let len = Array.length dancers in
    let spinpoint = len - n - 1
    let beforeSpin = dancers.[0..spinpoint] in
    let afterSpin = dancers.[(spinpoint+1)..] in
    Array.append afterSpin beforeSpin

let swap a b m =
    let valA = Map.find a m in
    let valB = Map.find b m in
    [(a, valB); (b, valA)]
    |> List.fold (fun m (a, b) -> Map.add a b m) m

let exchange a b (dancers : Dancers) =
    dancers
    |> Array.indexed
    |> Map.ofArray
    |> swap a b
    |> Map.toArray
    |> Array.map snd

let partner a b (dancers : Dancers) =
    dancers
    |> Array.indexed
    |> Array.map (fun (a, b) -> (b, a))
    |> Map.ofArray
    |> swap a b
    |> Map.toArray
    |> Array.sortBy snd
    |> Array.map fst

let parseCommand (input : string) =
    let cmd = input.[0] in
    let args = input.[1..] in
    let parseInt = Int64.Parse >> int
    match cmd with
    | 's' -> args |> parseInt |> spin
    | 'x' -> args.Split '/' |> Array.map parseInt |> (fun [|a;b|] -> exchange a b)
    | 'p' -> args.Split '/' |> Array.map (fun x -> x.ToCharArray().[0]) |> (fun [|a;b|] -> partner a b)

let parse (input : string) =
    input.Split ','
    |> Array.map parseCommand

let solve1 = parse >> Array.fold (|>) inputDancers >> String
let input = System.IO.File.ReadAllText "input.txt"

let iter n f a =
    Seq.init n id
    |> Seq.fold (fun a _ -> f a) a

let solve2 input =
   let cmds = parse input in
   iter 1_000 (fun str -> Array.fold (|>) str cmds) inputDancers


let () = printf "%s\n" (solve2 input |> String)
