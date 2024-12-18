
let checkTriangle (a, b, c) = a + b > c && a + c > b && b + c > a

let parseLine (line: string) =
   line.Split("  ")
   |> Array.map _.Trim()
   |> Array.filter (fun s -> s <> "" && s <> " ")
   |> Array.map int

let parseInput (input: string) =
   input.Split("\n")
   |> Array.map parseLine

let countValidTriangles (input: string) =
   input
   |> parseInput
   |> Array.filter (fun l -> checkTriangle (l[0], l[1], l[2]))
   |> Array.length

let countVerticleValidTriangles (input: string) =
   input
   |> parseInput
   |> Array.chunkBySize 3
   |> Array.map (fun l -> l |> Array.transpose |> Array.map (fun x -> x |> Array.toList) |> Array.map (fun l -> checkTriangle (l.[0], l.[1], l.[2])))
   |> Array.map (fun l -> l |> Array.filter id |> Array.length)
   |> Array.sum

let inputString = System.IO.File.ReadAllText "./input.txt"

let result1 = countValidTriangles inputString
printfn "Test result: %A" result1

let result2 = countVerticleValidTriangles inputString
printfn "Test result: %A" result2