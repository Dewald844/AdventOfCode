// For more information see https://aka.ms/fsharp-console-apps

open System
open System.IO
open Microsoft.FSharp.Core

type Elf = {
   Number   : int
   Calories : int
}

let elfL : List<Elf> = List.empty
let inputL = File.ReadLines("./input.txt")
let tryParseInt s = if s = "" then None else Some (int s)
let evolve () =
   inputL
   |> Seq.map tryParseInt
   |> Seq.fold (fun state elem ->
         match elem with
         | None -> []::state
         | Some calCount ->
            let newState =
               match state with
               | [x] -> [calCount::x]
               | y::z -> (calCount::y)::z
               | [] -> state
            newState

      ) [[]]


[<EntryPoint>]
let main args =

   let partOne =
      evolve ()
      |> List.map List.sum
      |> List.max

   let partTwo =
      evolve ()
      |> List.map List.sum
      |> List.sortDescending
      |> List.take 3
      |> List.sum

   printf $"Part 1 : {partOne}\n"
   printf $"Part 2 : {partTwo}\n"

   0