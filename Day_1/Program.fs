// Day 1: Trebuchet?!

open System
open Microsoft.FSharp.Core
open System.Text.RegularExpressions

let inputL = System.IO.File.ReadLines("../../../input.txt") |> List.ofSeq

let toIntO (s : string) =
   try
      Int32.Parse(s)
      |> Some
   with _ ->
      None

let test = ["1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet"]

let getNumForPartOne (inputL : List<string>) : List<int> =

   inputL
   |> List.map (fun input ->
      let stringOfNum =
         input
         |> Seq.map (fun s -> (toIntO (string s)))
         |> Seq.choose id
         |> Seq.map string
         |> String.concat ""
      let firstNum = stringOfNum[0] |> string
      let lastNum = stringOfNum[stringOfNum.Length - 1] |> string
      String.concat "" [firstNum; lastNum]
      |> int
   )

let part1 =
   getNumForPartOne inputL
   |> List.sum

// _________ Part 2 _________

let test2 = ["two1nine"; "eightwothree"; "abcone2threexyz"; "xtwone3four"; "4nineeightseven2"; "zoneight234"; "7pqrstsixteen"]

let extraction (input : string) =

   let wordToInt =
     ["one", 1; "two", 2; "three", 3; "four", 4; "five", 5; "six", 6; "seven", 7; "eight", 8; "nine", 9]
     |> Map.ofSeq

   let regex = "(one|two|three|four|five|six|seven|eight|nine)|[1-9]"
   let matches =
      Regex.Matches(input, regex)
      |> Seq.cast<Match>
      |> Seq.map (fun m -> m.Value)
      |> Seq.toArray

   let firstNum =
      match wordToInt |> Map.containsKey matches[0] with
      | true ->
         wordToInt
         |> Map.find matches[0]
         |> string
      | false -> matches[0]

   let lastNum =
      if matches.Length <= 1 then
         ""
      else
         match wordToInt |> Map.containsKey matches[matches.Length - 1] with
         | true ->
            wordToInt
            |> Map.find matches[matches.Length - 1]
            |> string
         | false ->
            matches[matches.Length - 1]

   [firstNum; lastNum]

let getNumForPartTwo (inputL : List<string>) : List<int> =

   inputL
   |> List.map extraction
   |> List.map (String.concat "")
   |> List.map int

let part2 () =
   getNumForPartTwo inputL
   |> List.sum

let part2Test =
   getNumForPartTwo test2
   |> List.sum

printf "==============================\n"
printf $"Part 1 test = {(getNumForPartOne test) |> List.sum}\n"
printf $"Part 1 = {part1}\n"
printf "==============================\n"
printf $"Part 2 test = {part2Test}\n"
printf $"Part 2 = {part2 ()}\n"
printf "==============================\n"
