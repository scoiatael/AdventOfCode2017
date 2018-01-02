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

let exchange a b (dancers : Dancers) =
    let valA = dancers.[a] in
      dancers.[a] <- dancers.[b]
    ; dancers.[b] <- valA
    ; dancers

let partner a b (dancers : Dancers) =
    let idxA = Array.findIndex ((=) a) dancers in
    let idxB = Array.findIndex ((=) b) dancers in
    exchange idxA idxB dancers

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
   iter 1_000_00 (fun str -> Array.fold (|>) str cmds) inputDancers


let () = printf "%s\n" (solve2 input |> String)