// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

type Position = {
  Horizontal : int
  Depth : int
  Aim : int
}

type Move =
  | Forward of int
  | Down of int
  | Up of int

module Move =
  let fromString (s,int) =
    match s, int with 
    | "forward", x -> Forward x
    | "up", x      -> Up x
    | "down", x    -> Down x
    | _ -> failwith "Unexpected input"
    
let testList =
  [
    "forward 5"
    "down 5"
    "forward 8"
    "up 3"
    "down 8"
    "forward 2"
  ]
    
let inputLines =
  System.IO.File.ReadLines "./input.txt" 
  |> Seq.toList
  |> List.map (fun x -> x.Split " ")
  |> List.map (fun x -> x.[0], int x.[1])
  
let calculate position move =
  match move with
  | Forward x -> { position with Horizontal = position.Horizontal + x
                                 Depth = (position.Aim * x) + position.Depth }
  | Up x      -> { position with Aim = position.Aim - x }
  | Down x    -> { position with Aim = position.Aim + x }
  
let initPosition =
  { Horizontal = 0
    Depth = 0
    Aim = 0 }
  
let result =   
  inputLines
  |> List.map Move.fromString
  |> List.fold calculate initPosition
 
let testResult =   
  testList
  |> List.map (fun x -> x.Split " ")
  |> List.map (fun x -> x.[0], int x.[1])
  |> List.map Move.fromString
  |> List.fold calculate initPosition
 
let testTotal = testResult.Horizontal * testResult.Depth
let resultTotal = result.Horizontal * result.Depth
  
printfn "RESULT : %A" resultTotal
printfn "TEST : %i" testTotal
  