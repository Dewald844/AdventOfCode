let inputL = System.IO.File.ReadLines("./input.txt") |> List.ofSeq

let testData =
   """3   4
4   3
2   5
1   3
3   9
3   3"""

let testL =
   inputL
   |> List.map (fun x -> x.Split("   ") |> Array.map int |> List.ofArray)
   |> List.map (fun a -> (a[0], a[1]))


let calculateDifference (a, b) = abs(a - b)

// Part 1
let result1 inputL =
   let rL = inputL |> List.map (fun (r, l) -> r) |> List.sortDescending
   let lL = inputL |> List.map (fun (r, l) -> l) |> List.sortDescending

   let rec loop rL lL acc =
      match rL, lL with
      | [], [] -> acc
      | r::rs, l::ls -> loop rs ls (acc + calculateDifference(r, l))
      | _, _ -> failwith "Invalid input"

   loop rL lL 0

// Part 1
result1 testL |> printfn "%A"

let result2 inputL =
   let rL = inputL |> List.map (fun (r, l) -> r)
   let lL = inputL |> List.map (fun (r, l) -> l)

   lL
   |> List.map (fun l ->
      rL
      |> List.filter (fun r -> r = l)
      |> List.length
      |> fun count -> l * count
   )
   |> List.sum

// Part 2
result2 testL |> printfn "%A"
