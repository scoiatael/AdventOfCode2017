let isValid1(line: string) = 
    let maxDuplicates =
        line.Split ' '
        |> Seq.groupBy id
        |> Seq.map (fun (_,b) -> Seq.length b)
        |> Seq.reduce max
    in maxDuplicates < 2
let lines(text: string) = text.Split '\n'
let solveWith pred text = text |> lines |> Seq.filter pred |> Seq.length
let solve1 = solveWith isValid1
let isValid2(line: string) = 
    let maxDuplicates =
        line.Split ' '
        |> Seq.groupBy (fun str -> str.ToCharArray() |> Array.sort)
        |> Seq.map (fun (_,b) -> Seq.length b)
        |> Seq.reduce max
    in maxDuplicates < 2
let solve2 = solveWith isValid2