// For more information see https://aka.ms/fsharp-console-apps
open System.IO

let inputL () =
   File.ReadLines("../../../input.txt")
   |> Seq.map (fun s ->
      let sA = s.Split(" ")
      sA[0], sA[1]
   )

type Choice = | Rock | Paper | Scissors

type Outcome =
   | Win  of int
   | Draw of int
   | Loss of int

let getOutcomeInt = function
   | Win  x | Draw x | Loss x -> x

let outcomeFn (opponentChoice, playerChoice) =
   match opponentChoice, playerChoice with
   | Rock    , Rock     -> Draw (1 + 3)
   | Rock    , Paper    -> Win  (2 + 6)
   | Rock    , Scissors -> Loss (3 + 0)
   | Paper   , Rock     -> Loss (1 + 0)
   | Paper   , Paper    -> Draw (2 + 3)
   | Paper   , Scissors -> Win  (3 + 6)
   | Scissors, Rock     -> Win  (1 + 6)
   | Scissors, Paper    -> Loss (2 + 0)
   | Scissors, Scissors -> Draw (3 + 3)

let choiceFromStringPart1 s =
   match s with
   | "A" -> Rock
   | "B" -> Paper
   | "C" -> Scissors
   | "X" -> Rock
   | "Y" -> Paper
   | "Z" -> Scissors
   | _ -> failwith "Unexpected string in input file"

let choiceFromStringPart2 (s, s2) =
     match s with
     | "A" ->
        let oppChoice = Rock
        let playerChoice =
            match s2 with
            | "X" -> Scissors
            | "Y" -> Rock
            | "Z" -> Paper
            | _ -> failwith "Invalid input"
        (oppChoice, playerChoice)
     | "B" ->
        let oppChoice = Paper
        let playerChoice =
            match s2 with
            | "X" -> Rock
            | "Y" -> Paper
            | "Z" -> Scissors
            | _ -> failwith "Invalid input"
        (oppChoice, playerChoice)
     | "C" ->
        let oppChoice = Scissors
        let playerChoice =
            match s2 with
            | "X" -> Paper
            | "Y" -> Scissors
            | "Z" -> Rock
            | _ -> failwith "Invalid input"
        (oppChoice, playerChoice)
     | _ -> failwith "Invalid input"


let inputPart1 =
   inputL ()
   |> Seq.toList
   |> List.map (fun (opp, player) -> (opp |> choiceFromStringPart1), (player |> choiceFromStringPart1))
   |> List.map outcomeFn
   |> List.map getOutcomeInt
   |> List.sum

let inputPart2 =
   inputL ()
   |> Seq.toList
   |> List.map choiceFromStringPart2
   |> List.map outcomeFn
   |> List.map getOutcomeInt
   |> List.sum

[<EntryPoint>]
let main args =
   printf $"Total 1 %A{inputPart1}"
   printf $"Total 2 %A{inputPart2}"
   0



