let testInput= """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"""

let inputString = System.IO.File.ReadAllText "./input.txt"

let createMatrix (input : string) =
   input.Split("\n")
   |> Array.map _.ToCharArray()
   |> Array.map (Array.map _.ToString())
   |> Array.map (fun x -> printf $"%A{x}\n"; x)

type Pos = { column : int; row : int }

let getAntennas (matrix : string[][]) =
   let mutable antennas = []
   for y in 0..matrix.Length - 1 do
      for x in 0..matrix[y].Length - 1 do
         if matrix[y][x] <> "." then
            antennas <- (matrix[y][x] , { column = x; row = y })::antennas
   antennas

let calculateDifference (a : Pos) (b : Pos) =
   (a.column - b.column) , (a.row - b.row)

let calculateAntiNodePosition (a : Pos) (b : Pos) =
   let diffX, diffY = calculateDifference a b
   { row = (a.row + diffY); column = (a.column + diffX) }

let rec findAntiNodesInMatrix (matrix : string[][]) (antennas : List<string * Pos>) (antiNodeL : List<Pos>) =
   let mutable antiNodeInnerL = []
   match antennas with
   | (antenna, pos)::tail ->
      for row in 0..matrix.Length - 1 do
         for column in 0..matrix[row].Length - 1 do
            if (matrix[row][column] = antenna) && (row <> pos.row && column <> pos.column) then
               let antiNode = calculateAntiNodePosition pos { column = column; row = row }
               antiNodeInnerL <- antiNode::antiNodeInnerL
      findAntiNodesInMatrix matrix tail (antiNodeL |> List.append antiNodeInnerL)
   | _ -> antiNodeL

let result1 (input : string) =
   let matrix = createMatrix input
   let antennas = getAntennas matrix
   let antinodeL = findAntiNodesInMatrix matrix antennas []

   let count =
      antinodeL
      |> List.distinct
      |> List.filter (fun p -> (p.column >=0 && p.row >= 0 && p.column < matrix[0].Length && p.row < matrix.Length))
      |> List.length

   printfn $"Part 1 result : {count}\n"

result1 inputString