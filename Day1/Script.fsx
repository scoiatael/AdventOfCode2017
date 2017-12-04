let index (word: 'a[], ind: int) = ind % word.Length
let sumOfDoubles forwardJump (word: int[]) =
    [0..word.Length-1]
    |> List.map (fun ind -> if word.[ind] = word.[index(word, ind+forwardJump)] then word.[ind] else 0)
    |> List.reduce (+)

let parse(word: string) = word.ToCharArray() |> Array.map (fun ch -> (int ch) - (int '0'))
let solve1(word: string) = parse word |> (sumOfDoubles 1)
let solve2(word: string) = parse word |> fun arr -> sumOfDoubles (arr.Length / 2) arr