

type Measurement = Measurement of  int64
type Calculation =
  | Increased
  | Decreased
  | Initial
type ListIndex = Index of int

type CalculatedMeasurement = {
  Measurement : Measurement
  Calculation : Calculation
  ListIndex   : ListIndex
}

let inputLines =
  seq{ yield! System.IO.File.ReadLines "./Puzzle1Input.txt" }
  |> Seq.toList
  |> List.map int64
let calculatedL =
  inputLines
  |> List.mapi (fun i x ->
    let previousIndex = i - 1
    if i = 0 then
      {
        Measurement = x |> Measurement.Measurement
        Calculation = Calculation.Initial
        ListIndex   = ListIndex.Index i
      }
    elif x > inputLines.[previousIndex] then
      {
        Measurement = x |> Measurement.Measurement
        Calculation = Calculation.Increased
        ListIndex   = ListIndex.Index i
      }
    else
      {
        Measurement = x |> Measurement.Measurement
        Calculation = Calculation.Decreased
        ListIndex   = ListIndex.Index i
      } )
  
let increasedNumber =
    calculatedL
    |> List.filter (fun x -> x.Calculation = Calculation.Increased)
    |> List.length
    
printf "Increased Count %i" increasedNumber
printf "%A"calculatedL
