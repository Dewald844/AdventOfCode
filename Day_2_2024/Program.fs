let testInput =
   """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9""".Split("\n")

let input = System.IO.File.ReadAllLines "./input.txt"

let prepareInput (input : string array) =
   input
   |> Array.map (fun x -> x.Split(" ") |> Array.map int |> List.ofArray)
   |> Array.map (fun a -> (a[0], a[1], a[2], a[3], a[4]))


let determineSafety (a, b, c, d, e) =
   let ab = a - b
   let bc = b - c
   let cd = c - d
   let de = d - e

   if ab > 0 && ab <= 2 then
      bc <= 2 && cd <= 2 && de <= 2
   elif ab < 0 && ab >= -2 then
      bc >= -2 && cd >= -2 && de >= -2
   else
      false


let result1 input = input |> prepareInput |> Array.filter determineSafety |> Array.length

// Part 1
result1 input |> printfn "%A"