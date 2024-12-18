
type Move =
   | Up
   | Right
   | Down
   | Left

   with
      static member fromString (s : char) =
         match s with
         | 'U' -> Up
         | 'R' -> Right
         | 'D' -> Down
         | 'L' -> Left
         | _ -> failwith "Invalid move"

type Position = { x : int; y : int }
   with
      member this.Move (move : Move) =
         match move with
         | Up    -> { this with y = this.y - 1 }
         | Right -> { this with x = this.x + 1 }
         | Down  -> { this with y = this.y + 1 }
         | Left  -> { this with x = this.x - 1 }

let testInput =
   """ULL
RRDDD
LURDL
UUUUD"""

let inputString = System.IO.File.ReadAllText "./input.txt"

let parseInput (input : string) =
   input.Split("\n")
   |> Array.map _.ToCharArray()

let findCodePart1 (input : char[][]) (start : Position) =

   let keypad = [|
      [| 1; 2; 3 |]
      [| 4; 5; 6 |]
      [| 7; 8; 9 |]
   |]

   let mutable position = start
   input
   |> Array.map (fun line ->
      line
      |> Array.map (fun move ->
         let newPosition = position.Move (Move.fromString move)
         if newPosition.x >= 0 && newPosition.x < keypad[0].Length && newPosition.y >= 0 && newPosition.y < keypad.Length then
            position <- newPosition
         keypad[position.y][position.x]
      )
   )
   |> Array.map (fun arr -> arr[arr.Length-1])


let findCodePart2 (input : char[][]) (start : Position) =

   let keypad2 = [|
      [| '.'; '.'; '1'; '.'; '.' |]
      [| '.'; '2'; '3'; '4'; '.' |]
      [| '5'; '6'; '7'; '8'; '9' |]
      [| '.'; 'A'; 'B'; 'C'; '.' |]
      [| '.'; '.'; 'D'; '.'; '.' |]
   |]

   let mutable position = start
   input
   |> Array.map (fun line ->
      line
      |> Array.map (fun move ->
         let newPosition = position.Move (Move.fromString move)
         if newPosition.x >= 0 && newPosition.x < keypad2[0].Length && newPosition.y >= 0 && newPosition.y < keypad2.Length && keypad2[newPosition.y].[newPosition.x] <> '.' then
            position <- newPosition
         keypad2[position.y][position.x]
      )
   )
   |> Array.map (fun arr -> arr[arr.Length-1])


let testResult = findCodePart1 (parseInput testInput) { x = 1; y = 1 }
printfn "Test result: %A" testResult

let result = findCodePart1 (parseInput inputString) { x = 1; y = 1 }
printfn "Code: %A" result

let testResult2 = findCodePart2 (parseInput testInput) { x = 0; y = 2 }
printfn "Test result: %A" testResult2

let result2 = findCodePart2 (parseInput inputString) { x = 0; y = 2 }
printfn "Code: %A" result2