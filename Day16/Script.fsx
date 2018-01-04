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
      let dancers = Array.copy dancers in
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
let addNSpin {listLength=listLength; spin=spin} b = (b - spin + listLength) % listLength
let exchangeAfterSpin g a b = (addNSpin g a, addNSpin g b)
let addSwap m a b =
    let valA = Map.find a m in
    let valB = Map.find b m in
    m
    |> Map.add a valB
    |> Map.add b valA

let optimize oc = function
    | (Spin n) -> {oc with spin=(addSpin oc n)}
    | (Exchange(a,b)) -> (let (newA, newB) = exchangeAfterSpin oc a b in {oc with exchange=(addSwap oc.exchange newA newB)})
    | (Partner(a,b)) -> {oc with partner=(addSwap oc.partner a b)}

let partnerMerge m1 m2 =
    Map.toList m1
    |> List.map (fun (k, v) -> (k, Map.find v m2))
    |> Map.ofList

let exchangeMerge g exchange =
    Map.toList exchange
    |> List.map (fun (a, b) -> exchangeAfterSpin g a b)
    |> Map.ofList

// Assume list lengths are the same
// This assumption should be probably moved to module level
let add oc {partner=partner; spin=spin; exchange=exchange} =
    let newPartner = partnerMerge oc.partner partner in
    let newSpin = addSpin oc spin in
    let newExchange = exchangeMerge oc exchange in
    {oc with partner=newPartner; spin=newSpin; exchange=newExchange}

let reverseOf m =
    m
    |> Map.toList
    |> List.map (fun (a, b) -> (b, a))
    |> Map.ofList

let executeOptimized d {spin=spin; partner=partner; exchange=exchange} =
    let executePartner  = fun ds ->
        let reversePartner = reverseOf partner in
        ds |> Array.map (fun d -> Map.find d reversePartner) in
    let executeExchange = fun ds ->
        let original = Array.copy ds in
        ds
        |> Array.indexed
        |> Array.map (fst >> fun idx -> original.[Map.find idx exchange]) in
    d |> executeExchange |> executePartner |> execute (Spin spin)

let optimizeSeqFor chrs = Seq.fold optimize (NewOptimizedCommand chrs)
let solve (cmds: Command seq) =
    cmds
    |> optimizeSeqFor inputDancers
    |> executeOptimized inputDancers
    |> String
let solve1 = parse >> solve
let input = System.IO.File.ReadAllText "input.txt"

let check input cmdSeq =
    let originalInput = Array.copy input in
    cmdSeq
    |> Seq.fold (fun (noc, mutated : char []) cmd ->
                 let fromOptimized = (executeOptimized originalInput noc |> String) in
                 let fromMutation = (mutated |> String) in
                 ( printf "%A on %A vs %A\n" cmd fromOptimized fromMutation
                 ; ((optimize noc cmd), (execute cmd mutated)))) (NewOptimizedCommand input, input)

let iter n a =
    Seq.init n id
    |> Seq.map (fun _ -> a)

let solve2 input =
   let cmds = parse input |> Seq.ofArray in
   let dancers = inputDancers in
   iter 1_000 cmds
   |> Seq.concat
   |> solve


let () = printf "%s\n" (solve2 input)
