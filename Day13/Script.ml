# require "str";;
type layer = { position : int; length: int}
type layerState = { wall: layer; state: int }

let (>>) f g x = g (f x)
let (|>) x f = f x
let reduce f xs = List.fold_left f (List.hd xs) (List.tl xs)
let read f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let stream_filter p stream =
    let rec next i =
      try
        let value = Stream.next stream in
        if p value then Some value else next i
      with Stream.Failure -> None in
    Stream.from next

let stream_map f stream =
    let rec next i =
      try Some (f (Stream.next stream))
      with Stream.Failure -> None in
    Stream.from next

let nonempty = String.length >> ((<) 0)

let parseLine =
  Str.split (Str.regexp ": ")
    >> List.map int_of_string
    >> (fun [x; y] -> { position=x; length=y})

let parse =
  String.split_on_char '\n'
    >> List.filter nonempty
    >> List.map parseLine

let example = "
0: 3
1: 2
4: 4
6: 4
"
let input = read "input.txt"

let advance n ({length} as wall) = {wall=wall; state = n mod (2*length-2)}
let hasCollision n {wall={position}; state} = position = n && state = 0
let position {position} = position
let solve  =
  parse
  >> List.filter (fun ({position} as wall) -> advance position wall |> hasCollision position)
  >> List.map (fun {position; length} -> position*length)
  >> List.fold_left (+) 0

let canPassWithDelay n =
  List.filter (fun ({position} as wall) -> advance (n + position) wall |> hasCollision position)
  >> List.length
  >> (=) 0

let rec filter_ints p from = if p from then from else filter_ints p (from + 1)

let solve2 =
  parse
  >> (fun firewall ->
      filter_ints (fun n -> canPassWithDelay n firewall) 0)
