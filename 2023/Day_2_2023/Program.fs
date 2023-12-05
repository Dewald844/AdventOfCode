// --- Day 2: Cube Conundrum ---

let inputL = System.IO.File.ReadLines("../../../input.txt") |> List.ofSeq

let test  = [
   "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
   "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
   "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
   "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
   "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
]

type Count = { Red: int; Green: int; Blue: int }

type Game = { GameNumber: int; Round: List<Count> }

let parseLine (line: string) =
   let gameNumber = line.Split(":").[0].Split(" ")[1] |> int
   let cubes =
      line.Split(": ").[1].Trim().Split("; ")
      |> Array.map (fun x ->
         let round = x.Trim().Split(", ")
         let redCubes   = round |> Array.filter (fun c -> (c.Split(" ")[1]).Trim() = "red" )
         let greenCubes = round |> Array.filter (fun c -> (c.Split(" ")[1]).Trim() = "green" )
         let blueCubes  = round |> Array.filter (fun c -> (c.Split(" ")[1]).Trim() = "blue" )
         let countCubes (cubes: string[]) =
            cubes
            |> Array.map (fun x -> x.Split(" ")[0] |> int)
            |> Array.sum

         { Red = redCubes |> countCubes; Green = greenCubes |> countCubes; Blue = blueCubes |> countCubes }
      )

   {
      GameNumber = gameNumber
      Round = cubes |> Array.toList
   }

let countPossibleGames (games : List<Game>) : List<int> =
   games
   |> List.filter (fun x ->
      x.Round
      |> List.forall (fun round ->
         round.Red <= 12 && round.Green <= 13 && round.Blue <= 14
         )
      )
   |> List.map (fun x -> x.GameNumber)

let calculatePart1 (input : List<string>) =
   input
   |> List.map parseLine
   |> countPossibleGames
   |> List.sum

//============== PART 2 ==============

let test2 = [
   "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
   "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
   "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
   "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
   "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
]

let parseLine2 (line: string) =
   let cubes =
      line.Split(": ").[1].Trim().Split("; ")
      |> Array.map(fun x -> x.Trim().Split(", "))
      |> Array.concat
      |> fun l -> printf $"Game : %A{l}"; l

   let lowestCubes =

         let lowestRed   =
            cubes
            |> Array.filter (fun x -> x.Split(" ").[1] = "red")
            |> Array.map (fun x -> x.Trim().Split(" ")[0] |> int)
            |> Array.sortByDescending id
            |> Array.tryHead
            |> Option.defaultValue 1

         let lowestGreen =
            cubes
            |> Array.filter (fun c -> (c.Split(" ").[1]) = "green" )
            |> Array.map (fun x -> x.Split(" ")[0] |> int)
            |> Array.sortByDescending id
            |> Array.tryHead
            |> Option.defaultValue 1

         let lowestBlue  =
            cubes
            |> Array.filter (fun c -> (c.Split(" ").[1]) = "blue" )
            |> Array.map (fun x -> x.Split(" ")[0] |> int)
            |> Array.sortByDescending id
            |> Array.tryHead
            |> Option.defaultValue 1

         printf $"Lowest red: {lowestRed}, Lowest green: {lowestGreen}, Lowest blue: {lowestBlue}\n"

         (lowestRed, lowestGreen, lowestBlue)


   lowestCubes
   |> fun (r, g, b) -> r * g * b

let calculatePart2 (input : List<string>) =
   input
   |> List.map parseLine2
   |> List.sum

let part1 = calculatePart1 inputL
let part1Test = calculatePart1 test

let part2 = calculatePart2 inputL
let part2Test = calculatePart2 test2


printf "===============================\n"
printf $"Part 1 test : {part1Test}\n"
printf $"Part 1 : {part1}\n"
printf "===============================\n"
printf $"Part 2 test : {part2Test}\n"
printf $"Part 2 : {part2}\n"
printf "===============================\n"



