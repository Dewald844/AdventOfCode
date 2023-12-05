// Part 1 day 1
// No time for a Taxi cab

type Direction =
   | North
   | South
   | East
   | West

type Move =
   | Left of int
   | Right of int

type Plot = {X: int; Y: int}

type Position = Direction * Plot

let calculate (pos: Position) (move: Move) =

   let currentDirection, position = pos

   match currentDirection with
   | North ->
      match move with
      | Left x  -> (West, {X = position.X - x; Y = position.Y})
      | Right x -> (East, {X = position.X + x; Y = position.Y})
   | South ->
      match move with
      | Left x  -> (East, {X = position.X + x; Y = position.Y})
      | Right x -> (West, {X = position.X - x; Y = position.Y})
   | East ->
      match move with
      | Left x  -> (North, {X = position.X; Y = position.Y + x})
      | Right x -> (South, {X = position.X; Y = position.Y - x})
   | West ->
      match move with
      | Left x  -> (South, {X = position.X; Y = position.Y - x})
      | Right x -> (North, {X = position.X; Y = position.Y + x})

let inline charToInt (c : char) = int (string c)

let input =
   System.IO.File.ReadAllText( "../../../input.txt").Split(",")
   |> List.ofArray
   |> List.map (fun s -> s.Trim())

let testL =
   //["R2"; "L3"]
   //["R2"; "R2"; "R2"]
   //["R5"; "L5"; "R5"; "R3"]
   ["R8"; "R4"; "R4"; "R8"]
   //input
   |> List.map (fun m ->
      if m[0] = 'R'
      then Right <| (int m[1..m.Length])
      else Left <| (int m[1..m.Length])
   )

// Part 1
let start1 = (North, {X = 0; Y = 0})
let endPos =
   List.fold calculate start1 testL
   |> snd
let numAway = abs ( abs endPos.X +  abs endPos.Y)

// Part 2

let populatePositions (positions : List<Plot>) instructions  =
   let rec loop posL (pos: Position) (moves: Move list) =
      match moves with
      | [] -> posL
      | h::t ->
         let newPos = calculate pos h
         let x = {X = abs (newPos |> snd).X; Y = abs (newPos |> snd).Y}
         loop (posL @ [(newPos |> snd)]) newPos t
   loop positions (North, {X = 0; Y = 0}) instructions

let firstDoubleVisit =
   testL
   |> populatePositions []
   |> fun l -> printf $"Total positions : {l.Length}\n\n %A{l} \n\n"; l
   |> List.groupBy id
   |> List.filter (fun (k, v) -> v.Length > 1)
   |> List.tryHead
   |> Option.map fst

let test2 =
   testL
   |> List.scan calculate (North, {X = 0; Y = 0})
   |> List.groupBy id
   |> List.filter (fun (k, v) -> v.Length > 1)
   |> List.tryHead
   |> Option.map fst

let numAway2 =
   match test2 with
   | Some (_,plot) -> abs plot.Y + abs plot.X
   | None ->
      0

printf "#############------------ PART 1 -------------#############\n"
printf $"End position: {endPos} \n block away : {numAway}\n\n"

printf "#############------------ PART 2 -------------#############\n"
printf $"First visited: {firstDoubleVisit} \n block away : {numAway2}\n\n"
