module Day16
open System

type Dancers = char array

let simpleDancers : Dancers = "abcde".ToCharArray()
let inputDancers : Dancers = "abcdefghijklmnop".ToCharArray()

type Command = Spin of int | Exchange of int*int | Partner of char*char

let rec execute = function
    | (Spin n) -> fun (dancers : Dancers)  ->
      let len = Array.length dancers in
      let spinpoint = len - n - 1
      let beforeSpin = dancers.[0..spinpoint] in
      let afterSpin = dancers.[(spinpoint+1)..] in
      Array.append afterSpin beforeSpin
    | (Exchange(a, b)) -> fun (dancers : Dancers) ->
      let valA = dancers.[a] in
        dancers.[a] <- dancers.[b]
      ; dancers.[b] <- valA
      ; dancers
    | (Partner(a, b)) -> fun (dancers : Dancers) ->
      let idxA = Array.findIndex ((=) a) dancers in
      let idxB = Array.findIndex ((=) b) dancers in
      execute (Exchange (idxA, idxB)) dancers

let parseCommand (input : string) =
    let cmd = input.[0] in
    let args = input.[1..] in
    let parseInt = Int64.Parse >> int
    match cmd with
    | 's' -> args |> parseInt |> Spin
    | 'x' -> args.Split '/' |> Array.map parseInt |> (fun [|a;b|] -> Exchange(a, b))
    | 'p' -> args.Split '/' |> Array.map (fun x -> x.ToCharArray().[0]) |> (fun [|a;b|] -> Partner(a, b))

let parse (input : string) =
    input.Split ','
    |> Array.map parseCommand

type OptimizedCommand = {partner: Map<char, char>; spin: int; exchange: Map<int, int>; listLength: int}
let NewOptimizedCommand chrs =
    let len = (Array.length chrs) in
    { partner= chrs |> Array.map (fun a -> (a,a)) |> Map.ofArray
    ; spin=0
    ; exchange= [0..len-1] |> List.map (fun a -> (a,a)) |> Map.ofList
    ; listLength= len}

let addSpin {listLength=listLength; spin=spin} b = (b + spin) % listLength
let swapAfterSpin g a b = (addSpin g a, addSpin g b)
let addSwap m a b =
    let valA = Map.find a m in
    let valB = Map.find b m in
    m
    |> Map.add a valB
    |> Map.add b valA

let optimize oc = function
    | (Spin n) -> {oc with spin=(addSpin oc n)}
    | (Exchange(a,b)) -> (let (newA, newB) = swapAfterSpin oc a b in {oc with exchange=(addSwap oc.exchange newA newB)})
    | (Partner(a,b)) -> {oc with partner=(addSwap oc.partner a b)}

let executeOptimized d {spin=spin; partner=partner; exchange=exchange} =
    let afterPartner  =
        Map.toSeq partner
        |> Seq.fold (fun d (a,b) -> execute (Partner(a,b)) d) d in
    let afterExchange =
        Map.toSeq exchange
        |> Seq.fold (fun d (a,b) -> execute (Exchange (a,b)) d) afterPartner in
    afterPartner |> execute (Spin spin)

let optimizeSeqFor chrs = Seq.fold optimize (NewOptimizedCommand chrs)
let solve = optimizeSeqFor inputDancers // >> executeOptimized inputDancers >> String
let solve1 = parse >> solve
let input = System.IO.File.ReadAllText "input.txt"

let check input cmdSeq =
    let originalInput = Array.copy input in
    cmdSeq
    |> Seq.fold (fun (noc, mutated : char []) cmd ->
                 ( printf "%A on %A vs %A\n" cmd (executeOptimized originalInput noc |> String) (mutated |> String)
                 ; ((optimize noc cmd), (execute cmd mutated)))) (NewOptimizedCommand input, input)

// let iter n f a =
//     Seq.init n id
//     |> Seq.fold (fun a _ -> f a) a

// let solve2 input =
//    let cmds = parse input in
//    iter 1_000_00 (fun str -> Array.fold (solve) str cmds) inputDancers


// let () = printf "%s\n" (solve2 input |> String)
