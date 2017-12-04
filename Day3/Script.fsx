let move (x, y) move_type = 
    match move_type with
    | 4 -> (x, y+1)
    | 3 -> (x-1, y)
    | 2 -> (x, y-1)
    | _ -> (x+1, y)
let next_pos (num, layer, pos) =
    let side_size = layer * 2 + 1 in
    let side_end = 8*(layer * (layer+1))/2 in
    let side_step = side_size - 1 in
    let card = (side_end - num) in
    let new_layer = layer + (if card = 0 then 1 else 0)
    let new_pos = move pos ((card+side_step-1) / side_step) in 
    Some(new_pos, (num+1, new_layer, new_pos))
let take_pos num = Seq.unfold next_pos (1, 1, (1, 0)) |> Seq.take(num - 1) |> Seq.last
let solve1 num = 
    let (x, y) = take_pos (num-1)
    in (abs x) + (abs y)
let neighbours (px,py) = 
    seq { for x in [|-1; 0; 1|] do
            for y in [| -1; 0; 1|] do
                yield (x, y)
    } |> Seq.map (fun (dx, dy) -> (px+dx, py+dy))
let vals_at board pos = Map.tryFind pos board |> Option.defaultValue 0
let fill (pos, board) =
    let v = pos |> neighbours |> Seq.sumBy (vals_at board) in
    v
let fillBoard (board, positions) =
    let pos = Seq.head positions in
    let value = fill (pos, board) in
    let new_board = Map.add pos value board in
    Some(value, (new_board, Seq.tail positions))
let solve2 num = 
    let positions = Seq.unfold next_pos (1, 1, (1, 0)) in
    let startMap = Map.empty |> Map.add (0,0) 1 |> Map.add (1,0) 1
    Seq.unfold fillBoard (startMap, positions)
    |> Seq.find (fun v -> v > num)