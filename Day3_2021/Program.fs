
let sampleInput = System.IO.File.ReadLines "./input.txt" |> Seq.toList
// power  = gamma * epsilon

let rec oxygenGenerator (inputList : List<string>) (counter : int) =
  if counter < 12 && inputList.Length > 1 then
    let ones = inputList |> List.filter (fun x -> x.[counter] = '1')
    let zeros = inputList |> List.filter (fun x -> x.[counter] = '0')
    if ones.Length > zeros.Length || ones.Length = zeros.Length
    then
      printf $"OXYGEN GENERATOR : Ones : %A{ones}\n"
      oxygenGenerator ones (counter + 1) 
    else
      printf $"OXYGEN GENERATOR : Zeros : %A{zeros}\n"
      oxygenGenerator zeros (counter + 1) 
  else inputList
  
let rec c02ScrubberRating (inputList : List<string>) (counter : int) =
  if counter < 12 && inputList.Length > 1 then 
    let ones = inputList |> List.filter (fun x -> x.[counter] = '1')
    let zeros = inputList |> List.filter (fun x -> x.[counter] = '0')
    if ones.Length > zeros.Length || ones.Length = zeros.Length
    then
      printf $"C02 SCRUBBER RATING ZEROS : %A{zeros}\n"
      c02ScrubberRating zeros (counter + 1)
    else
      printf $"C02 SCRUBBER RATING ONES : %A{ones}\n"
      c02ScrubberRating ones (counter + 1)
  else
    inputList
    
let oxygen = oxygenGenerator sampleInput 0
let c02Scrubber = c02ScrubberRating sampleInput 0

printf $"OXYGEN GENERATOR : %A{oxygen}\n"
printf $"C02 SCRUBBER RATING : %A{c02Scrubber}\n"
