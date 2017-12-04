open System
let parse(word: string) = 
    word.Split('\n') 
    |> Array.map (
        fun line ->
            line.Split(' ') 
            |> Array.map (Int64.Parse >> int))

let divideIfClean (dividend: int) (divisor: int) = 
    if dividend > divisor && dividend % divisor = 0 
    then Some (dividend / divisor) 
    else None
let findCleanDivide options dividend =
    options
    |> Array.map (divideIfClean dividend)
    |> Array.tryFind Option.isSome
    |> Option.bind id
let magicNumber(line: int[]) =
    line
    |> Array.map (findCleanDivide line)
    |> Array.find Option.isSome
    |> fun (Some v) -> v
let magicNumberSum(arr: int[][]) =
    arr
    |> Array.map magicNumber
    |> Array.reduce (+)
let checksum(arr: int[][]) = 
    arr
    |> Array.map (fun line -> (Array.reduce max line) - (Array.reduce min line)) 
    |> Array.reduce (+)

let solve1(word: string) = word |> parse |> checksum
let solve2(word: string) = word |> parse |> magicNumberSum