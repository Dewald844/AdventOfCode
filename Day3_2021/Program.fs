

open System.Runtime.Serialization

let sampleInput = System.IO.File.ReadLines "./input.txt" |> Seq.toList
// power  = gamma * epsilon

let index1L = sampleInput |> List.map (fun x -> x.[0])
let index2L = sampleInput |> List.map (fun x -> x.[1])
let index3L = sampleInput |> List.map (fun x -> x.[2])
let index4L = sampleInput |> List.map (fun x -> x.[3])
let index5L = sampleInput |> List.map (fun x -> x.[4])
let index6L = sampleInput |> List.map (fun x -> x.[5])
let index7L = sampleInput |> List.map (fun x -> x.[6])
let index8L = sampleInput |> List.map (fun x -> x.[7])
let index9L = sampleInput |> List.map (fun x -> x.[8])
let index10L = sampleInput |> List.map (fun x -> x.[9])
let index11L = sampleInput |> List.map (fun x -> x.[10])
let index12L = sampleInput |> List.map (fun x -> x.[11])
    
let gammaOneOrZero list =
  let ones  = list |> List.filter (fun x -> x = '1') |> List.length
  let zeros = list |> List.filter (fun x -> x = '0') |> List.length
  if ones > zeros then 1
  elif ones = zeros then 6
  else 0
  
let epsilonOneOrZero list =
  let ones  = list |> List.filter (fun x -> x = '1') |> List.length
  let zeros = list |> List.filter (fun x -> x = '0') |> List.length
  if ones > zeros then 0
  elif ones = zeros then 6
  else 1
  
let gamma1Bit = index1L   |> gammaOneOrZero
let gamma2Bit = index2L   |> gammaOneOrZero
let gamma3Bit = index3L   |> gammaOneOrZero
let gamma4Bit = index4L   |> gammaOneOrZero
let gamma5Bit = index5L   |> gammaOneOrZero
let gamma6Bit = index6L   |> gammaOneOrZero
let gamma7Bit = index7L   |> gammaOneOrZero
let gamma8Bit = index8L   |> gammaOneOrZero
let gamma9Bit = index9L   |> gammaOneOrZero
let gamma10Bit = index10L |> gammaOneOrZero
let gamma11Bit = index11L |> gammaOneOrZero
let gamma12Bit = index12L |> gammaOneOrZero

let epsilon1Bit = index1L  |> epsilonOneOrZero
let epsilon2Bit = index2L  |> epsilonOneOrZero
let epsilon3Bit = index3L  |> epsilonOneOrZero
let epsilon4Bit = index4L  |> epsilonOneOrZero
let epsilon5Bit = index5L  |> epsilonOneOrZero
let epsilon6Bit = index6L  |> epsilonOneOrZero
let epsilon7Bit = index7L  |> epsilonOneOrZero
let epsilon8Bit = index8L  |> epsilonOneOrZero
let epsilon9Bit = index9L  |> epsilonOneOrZero
let epsilon10Bit = index10L |> epsilonOneOrZero
let epsilon11Bit = index11L |> epsilonOneOrZero
let epsilon12Bit = index12L |> epsilonOneOrZero



printfn $"GAMMA :{gamma1Bit}{gamma2Bit}{gamma3Bit}{gamma4Bit}{gamma5Bit}{gamma6Bit}{gamma7Bit}{gamma8Bit}{gamma9Bit}{gamma10Bit}{gamma11Bit}{gamma12Bit}"
printfn $"EPSILON :{epsilon1Bit}{epsilon2Bit}{epsilon3Bit}{epsilon4Bit}{epsilon5Bit}{epsilon6Bit}{epsilon7Bit}{epsilon8Bit}{epsilon9Bit}{epsilon10Bit}{epsilon11Bit}{epsilon12Bit}"


// 419 * 3676 1540244