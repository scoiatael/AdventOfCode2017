open System
type Cmd = Inc | Dec
type Cond = Gt | Lt | Gte | Lte | Eq | Neq
type CondWithArgs = {cond: Cond; srcReg: string; constCmp: int}
type CmdWithArgs = {cmd: Cmd; dstReg: string; constVal: int}
type CmdLine = CmdWithArgs * CondWithArgs
let parseCmd op dst v =
    let instr = match op with
    | "inc" -> Inc
    | "dec" -> Dec
    in {cmd=instr; constVal=(Int64.Parse v |> int); dstReg=dst}
let parseIf op a b = 
    let instr = match op with
    | "<" -> Gt
    | ">" -> Lt
    | "<=" -> Gte
    | ">=" -> Lte
    | "==" -> Eq
    | "!=" -> Neq
    in {cond=instr; srcReg=a; constCmp=(b |> Int64.Parse |> int)}
let parse (line: string) = 
    let instr = line.Split ' ' in
    (parseCmd instr.[1] instr.[0] instr.[2],
     parseIf instr.[5] instr.[4] instr.[6])

type State = Map<string, int>
let check {cond=cond; srcReg=srcReg; constCmp=constCmp} st =
    let v = Map.tryFind srcReg st |> Option.defaultValue 0 in
    match cond with
    | Gt -> v < constCmp
    | Lt -> v > constCmp
    | Gte -> v <= constCmp
    | Lte -> v >= constCmp
    | Eq -> v = constCmp
    | Neq -> v <> constCmp
let execute {cmd=cmd; dstReg=dstReg; constVal=constVal} st =
    let oldVal = Map.tryFind dstReg st |> Option.defaultValue 0 in
    let newVal = match cmd with
        | Inc -> oldVal + constVal
        | Dec -> oldVal - constVal
        in Map.add dstReg newVal st
let parseProgram(program: string) = program.Split('\n') |> Seq.ofArray |> Seq.map parse 
let maxVal = Map.toSeq >> Seq.map snd >> Seq.fold max (int Int64.MinValue)
let solve1 = parseProgram >> Seq.fold (fun st (cmd, cond) -> if check cond st then execute cmd st else st) Map.empty >> maxVal
let flipCons (a, b) = Seq.append [b] a
let solve2 = parseProgram >> Seq.mapFold (fun st (cmd, cond) -> (st, if check cond st then execute cmd st else st)) Map.empty >> flipCons >> Seq.map maxVal >> Seq.reduce max