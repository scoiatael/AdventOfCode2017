type StreamChunk = OpenBracket | CloseBracket | Garbage
type ParseState = InGroup | InGarbage | Escaped
let parse = function
    | InGroup -> function
        | "<" -> (None, InGarbage)
        | "," -> (None, InGroup)
        | "{" -> (Some OpenBracket, InGroup)
        | "}" -> (Some CloseBracket, InGroup)
    | InGarbage -> function
        | "!" -> (None, Escaped)
        | ">" -> (None, InGroup)
        | _ -> (Some(Garbage), InGarbage)
    | Escaped -> function _ -> (None, InGarbage)
let explode (stream: string) = [0..stream.Length-1] |> Seq.map (fun index -> stream.[index..index])
let clean =  explode >> Seq.mapFold parse InGroup >> fst >> Seq.filter Option.isSome >> Seq.map Option.get
let groupVals = function
 | OpenBracket -> 1 
 | CloseBracket -> -1
 | Garbage -> 0
let sumVals st v = let newV = if v > 0 then st + v else 0 in (newV, st+v)
let solve1 = clean >> Seq.map groupVals >> Seq.mapFold sumVals 0 >> fst >> Seq.reduce (+)
let input = System.IO.File.ReadAllText "Day9\\input.txt"
let isGarbage = function
    | Garbage -> true
    | _ -> false
let solve2 = clean >> Seq.filter isGarbage >> Seq.length