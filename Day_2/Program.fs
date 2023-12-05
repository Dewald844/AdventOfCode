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

type Game = { GameNumber: int; Cubes: Count }

let parseLine (line: string) =
   let gameNumber = line.Split(':').[0].Split(' ')[1] |> int
   let cubes = line.Split(':').[1].Split(';')
   let count =
      cubes
      |> Array.map (fun x -> x.Trim().Split(", "))
      |> Array.map (fun x ->
         let first = x[0].Split(' ')
         let second = x[1].Split(' ')
         [
            (first.[1], int first.[0])
            (second.[1], int second.[0])
         ] |> List.toArray
      )
      |> Array.concat
   let red = count |> Array.filter (fun (x, _) -> x = "red") |> Array.map snd |> Array.sum
   let green = count |> Array.filter (fun (x, _) -> x = "green") |> Array.map snd |> Array.sum
   let blue = count |> Array.filter (fun (x, _) -> x = "blue") |> Array.map snd |> Array.sum
   { GameNumber = gameNumber; Cubes = { Red = red; Green = green; Blue = blue } }

let countPossibleGames (games : List<Game>) : List<int> =
   games
   |> List.filter (fun x -> x.Cubes.Red <= 12 && x.Cubes.Green <= 13 && x.Cubes.Blue <= 14)
   |> List.map (fun x -> x.GameNumber)

let part1Test = test |> List.map parseLine |> countPossibleGames |> List.sum

printf "---------------------\n"
printf $"Part 1 test : {part1Test}\n"

