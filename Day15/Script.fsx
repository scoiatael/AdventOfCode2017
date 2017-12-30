module Day15

type Generator = {factor: bigint; seed: bigint; modulus: bigint}

let advance ({factor=factor; seed=seed; modulus=modulus} as generator) =
    let newValue = (seed * factor) % modulus in
    (int newValue, {generator with seed=newValue})

let A = {factor= bigint 16807; seed= bigint 65; modulus= bigint 2147483647}
let B = {factor= bigint 48271; seed= bigint 8921; modulus= bigint 2147483647}

let powi a b =  (float a)**(float b) |> int

let pairApply f (a, b) = (f a, f b)

let mapFoldPair f pair =
    let ((aValue, newA), (bValue, newB)) = pairApply f pair in
    ((aValue, bValue), (newA, newB))

let updateProgress max i =
    let b = 1_000_000
    if i % b = 0 then printf "%2d/%2d\n" (i / b) (max / b) else ()

let mask16 i = i % (powi 2 16)

let InputA = {A with seed=(bigint 883)}
let InputB = {B with seed=(bigint 879)}

let solve1 () =
    let max = 40_000_000 in
    Seq.init max id
    |> Seq.mapFold (fun state i -> updateProgress max i; mapFoldPair advance state) (InputA, InputB)
    |> fst
    |> Seq.map (pairApply mask16 >> fun (a, b) -> a = b)
    |> Seq.filter id
    |> Seq.length

type PickyGenerator = {generator: Generator; filter: int}
let rec pickyAdvance ({generator=generator; filter=filter} as pickyGenerator) =
    let (newV, newG) = advance generator in
    let newGenerator = {pickyGenerator with generator=newG} in
    if newV % filter = 0
    then (newV, newGenerator)
    else pickyAdvance newGenerator


let PickyA = {generator=A; filter=4}
let PickyB = {generator=B; filter=8}

let solve2 () =
    let max = 5_000_000 in
    let start = ({PickyA with generator=InputA}, {PickyB with generator=InputB})
    Seq.init max id
    |> Seq.mapFold (fun state i -> updateProgress max i; mapFoldPair pickyAdvance state) start
    |> fst
    |> Seq.map (pairApply mask16 >> fun (a, b) -> a = b)
    |> Seq.filter id
    |> Seq.length

// Run with fsharpc --fullpaths Script.fsx; and time mono ./Script.exe solve1
[<EntryPoint>]
let main args =
    match args with
    | [|"solve1"|] -> solve1() |> printf "%d\n"; 0
    | [|"solve2"|] -> solve2() |> printf "%d\n"; 0
    | _ -> printf "Usage: solve1 | solve2"; 1
