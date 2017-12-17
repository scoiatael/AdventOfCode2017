let parseItem = function
 | "n" -> (0,2)
 | "ne" -> (1,1)
 | "nw" -> (-1,1)
 | "s" -> (0,-2)
 | "sw" -> (-1,-1)
 | "se" -> (1,-1)
let parseList (list: string) = list.Split(',') |> Array.map parseItem
// x >=0, y >= 0
let distanceFrom (x, y) = 
    if x <= y
    // Easy. Go left, then down
    // Each step down takes us 2 steps
    then x + (y-x)/2
    // Harder. Going left requires 2 steps 
    else x
let makeAbs (x,y) = (abs x, abs y)
let sumPair (ax, ay) (bx, by) = (ax + bx, ay + by)
let solve1 = parseList >> Array.reduce sumPair >> makeAbs >> distanceFrom
let input = System.IO.File.ReadAllText "Day11\\input.txt"
let double f a b = let y = f a b in (y,y)
let solve2 = parseList >> Array.mapFold (double sumPair) (0,0) >> fst >> Array.map distanceFrom >> Array.reduce max