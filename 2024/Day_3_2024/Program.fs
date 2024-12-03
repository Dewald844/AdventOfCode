open System.Text.RegularExpressions

let inputL = System.IO.File.ReadAllLines "./input.txt"
let testInput1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
let testInput2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

let buildString (input: string array) =
   let mutable newString = ""
   for i in input do newString <- newString + i
   newString

let interpretStringPart1 (input: string) =
   let pattern = @"mul\(\d+,\d+\)"
   let matches = Regex.Matches(input, pattern)

   let mutable sumL = []

   for m in matches do
      let value =
         m.Value.Split("mul(")
         |> fun x -> x[1].Split(",")
         |> fun x -> (int x[0], int (x[1].Split(")")[0]))
         |> fun (a, b) -> a * b

      sumL <- sumL @ [value]

   sumL
   |> List.sum


let interpretStringPart2 (input: string) =
   let pattern = @"mul\(\d+,\d+\)|do\(\)|don't\(\)"
   let matches = Regex.Matches(input, pattern)

   let mutable active = true

   let mutable sumL = []

   for m in matches do
      match m.Value with
      | "do()" -> active <- true
      | "don't()" -> active <- false
      | _ ->
         if active then
            let value =
               m.Value.Split("mul(")
               |> fun x -> x[1].Split(",")
               |> fun x -> (int x[0], int (x[1].Split(")")[0]))
               |> fun (a, b) -> a * b

            sumL <- sumL @ [value]

   sumL |> List.sum

let result1 = interpretStringPart1 (buildString inputL)
let result2 = interpretStringPart2 (buildString inputL)

// Part 1
printfn $"Part 1 | %A{result1}"
// Part 2
printfn $"Part 2 | %A{result2}"
