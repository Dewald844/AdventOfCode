let testInput =
   """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9""".Split("\n")

let inputL = System.IO.File.ReadAllLines "./input.txt"

let prepareInput (input : string array) =
   input
   |> Array.map (fun x -> x.Split(" ") |> Array.map int |> List.ofArray)
   |> Array.toList

let determineSafety (levelL : List<int>) =
   levelL
   |> List.mapi (fun i x ->
      if i = 0 then true
      else
         let r = levelL[i - 1] - x
         match (levelL[0] - levelL[1]) > 0 * -1 with
         | true -> r > 0 && r <= 3
         | false -> r < 0 && r >= -3
      )
   |> List.exists (fun x -> x = false)

let determineSafetyWithDampener (levelL : List<int>) : bool =

   let rec removeOneLevel (levelL : List<int>) counter length =
      if counter < length then
         let isUnsafe = List.removeAt counter levelL |> determineSafety
         if isUnsafe then
            removeOneLevel levelL (counter + 1) length
         else false
      else true

   if levelL |> determineSafety then
      removeOneLevel levelL 0 (List.length levelL)
   else false

let result1 input =
   input
   |> prepareInput
   |> List.map determineSafety
   |> List.filter (fun x -> x = false)
   |> List.length

let result2 input =
   input
   |> prepareInput
   |> List.map determineSafetyWithDampener
   |> List.filter (fun x -> x = false)
   |> List.length


// Part 1
result1 testInput |> printfn "Part 1 : %A"
// Part 2
result2 inputL |> printfn "Part 2 : %A"