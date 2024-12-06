// For more information see https://aka.ms/fsharp-console-apps
let testInput = [
   "MMMSXXMASM"
   "MSAMXMSMSA"
   "AMXSXMAAMM"
   "MSAMASMSMX"
   "XMASAMXAMM"
   "XXAMMXXAMA"
   "SMSMSASXSS"
   "SAXAMASAAA"
   "MAMMMXMMMM"
   "MXMXAXMASX"
]

let inputL =
   System.IO.File.ReadAllLines "./input.txt"
   |> Array.map (fun x -> x.ToCharArray() |> Array.toList)
   |> Array.toList

let test2D =
   testInput
   |> List.map (fun x -> x.ToCharArray() |> Array.toList)

let horizontalSearch (input: List<List<char>>) =
   let mutable count = 0
   for i in 0..input.Length - 1 do
      for j in 0..input[i].Length - 1 do
         if j + 3 <= input[i].Length - 1 then
            // Forward
            if input[i].[j] = 'X' && input[i].[j+1] = 'M' && input[i].[j+2] = 'A' && input[i].[j+3] = 'S'
            then
               count <- count + 1
            // Backward
            if input[i].[j] = 'S'
               && input[i].[j+1] = 'A'
               && input[i].[j+2] = 'M'
               && input[i].[j+3] = 'X'
            then
               count <- count + 1
   count

let verticalSearch (input : List<List<char>>) =
   let mutable count = 0
   for i in 0..input.Length - 1 do
      for j in 0..input[i].Length - 1 do
         if i + 3 <= input.Length - 1 then
            // Downward
            if input[i].[j] = 'X' && input[i+1].[j] = 'M' && input[i+2].[j] = 'A' && input[i+3].[j] = 'S'
            then
               count <- count + 1
            // Upward
            if input[i].[j] = 'S' && input[i+1].[j] = 'A' && input[i+2].[j] = 'M' && input[i+3].[j] = 'X'
            then
               count <- count + 1
   count

let rightDiagonalSearch (input : List<List<char>>) =
   let mutable count = 0
   for i in 0..input.Length - 1 do
      for j in 0..input[i].Length - 1 do
         if i + 3 <= input.Length - 1 && j + 3 <= input[i].Length - 1 then
            // Downward
            if input[i].[j] = 'X' && input[i+1].[j+1] = 'M' && input[i+2].[j+2] = 'A' && input[i+3].[j+3] = 'S'
            then
               count <- count + 1
            // Upward
            if input[i].[j] = 'S' && input[i+1].[j+1] = 'A' && input[i+2].[j+2] = 'M' && input[i+3].[j+3] = 'X'
            then
               count <- count + 1
   count

let leftDiagonalSearch (input : List<List<char>>) =
   let mutable count = 0
   for i in 0..input.Length - 1 do
      for j in 0..input[i].Length - 1 do
         if i + 3 <= input.Length - 1 && j - 3 >= 0 then
            // Downward
            if input[i].[j] = 'X' && input[i+1].[j-1] = 'M' && input[i+2].[j-2] = 'A' && input[i+3].[j-3] = 'S'
            then
               count <- count + 1
            // Upward
            if input[i].[j] = 'S' && input[i+1].[j-1] = 'A' && input[i+2].[j-2] = 'M' && input[i+3].[j-3] = 'X'
            then
               count <- count + 1
   count

let upAndDownDiagonalSearch (input : List<List<char>>) =
   let mutable count = 0
   for i in 0..input.Length - 1 do
      for j in 0..input[i].Length - 1 do
         if input[i][j] = 'A' && (i-1 >= 0 && i+1 <= input.Length - 1) && (j-1 >= 0 && j+1 <= input[i].Length - 1) then
            if ((input[i-1][j-1] = 'M' && input[i+1][j+1] = 'S') || (input[i-1][j-1] = 'S' && input[i+1][j+1] = 'M'))
               && ((input[i+1][j-1] = 'M' && input[i-1][j+1] = 'S') || (input[i+1][j-1] = 'S' && input[i-1][j+1] = 'M'))
            then
               count <- count + 1
   count

let resultPartOne inputL =

   printfn "Part One"
   printfn "----------------------+-----"
   printfn $"Horizontal search     | %A{horizontalSearch inputL}"
   printfn $"Vertical search       | %A{verticalSearch inputL}"
   printfn $"Right Diagonal search | %A{rightDiagonalSearch inputL}"
   printfn $"Left Diagonal search  | %A{leftDiagonalSearch inputL}"
   printfn "----------------------+-----"
   printfn $"Total                 | %A{horizontalSearch inputL + verticalSearch inputL + rightDiagonalSearch inputL + leftDiagonalSearch inputL}\n\n"

let resultPartTwo inputL =
   printfn "Part two"
   printfn "----------------------+-----"
   printfn $"Up and Down Diagonal search | %A{upAndDownDiagonalSearch inputL}\n\n"


do resultPartOne inputL
do resultPartTwo inputL