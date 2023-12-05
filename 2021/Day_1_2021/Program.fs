// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

type Calculation = Increased | Decreased | Initial | NoChange

let inputLines =
  System.IO.File.ReadLines "./Puzzle1Input.txt"
  |> Seq.toList
  |> List.map int64
  
let calculatedL input =
  input
  |> List.mapi (fun i x ->
    if i = 0               then Calculation.Initial
    elif x > input.[i - 1] then Calculation.Increased
    elif x = input.[i - 1] then Calculation.NoChange
    else Calculation.Decreased )
  
let increasedNumber =
  calculatedL inputLines
  |> List.filter (fun x -> x = Calculation.Increased)
  |> List.length
  
let calculatedByGroup =
  inputLines
  |> List.windowed 3
  |> List.map (fun x -> x |> List.sum)
    
let increasedNumberByGroup =
  calculatedL calculatedByGroup
  |> List.filter ( fun x -> x = Calculation.Increased)
  |> List.length
    
printf "Increased Count %i\n" increasedNumber
printf "Increased Count By Group %i\n" increasedNumberByGroup
