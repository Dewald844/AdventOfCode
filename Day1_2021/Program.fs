// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

type Measurement = Measurement of  int64
type Calculation = | Increased | Decreased | Initial | NoChange
type ListIndex = Index of int

type CalculatedMeasurement = {
  Measurement : Measurement
  Calculation : Calculation
  ListIndex   : ListIndex
}

let testL =
  [
    int64 199
    int64 200
    int64 208
    int64 210
    int64 200
    int64 207
    int64 240
    int64 269
    int64 260
    int64 263
  ]

let inputLines =
  seq{ yield! System.IO.File.ReadLines "./Puzzle1Input.txt" }
  |> Seq.toList
  |> List.map int64
  
let calculatedL input =
  printf "%i" (input |> List.length)
  input
  |> List.mapi (fun i x ->
    if i = 0 then
      {
        Measurement = int64 x |> Measurement.Measurement
        Calculation = Calculation.Initial
        ListIndex   = ListIndex.Index i
      }
    elif x > input.[i - 1] then
      {
        Measurement = int64 x |> Measurement.Measurement
        Calculation = Calculation.Increased
        ListIndex   = ListIndex.Index i
      }
    elif x = input.[i - 1] then
      {
        Measurement = int64 x |> Measurement.Measurement
        Calculation = Calculation.NoChange
        ListIndex   = ListIndex.Index i
      }
    else
      {
        Measurement = int64 x |> Measurement.Measurement
        Calculation = Calculation.Decreased
        ListIndex   = ListIndex.Index i
      } )
  
let calculatedByGroup =
  inputLines
  |> List.windowed 3
  |> List.map (fun x -> x |> List.sum)
  
let increasedNumber =
    calculatedL inputLines
    |> List.filter (fun x -> x.Calculation = Calculation.Increased)
    |> List.length
    
let increasedNumberByGroup =
  calculatedL calculatedByGroup
  |> List.filter ( fun x -> x.Calculation = Calculation.Increased)
  |> List.length
    
printf "Increased Count %i\n" increasedNumber
printf "Increased Count By Group %i\n" increasedNumberByGroup
