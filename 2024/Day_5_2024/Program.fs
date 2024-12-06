open System
open System.IO

let testInput = """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""

let inputL = File.ReadAllText "./input.txt"

type Before = Before of int
type OrderL = OrderL of List<int * Before>
type PrintL = PrintL of List<List<int>>

let parseOrderingL (s : string) =
   s.Split("\n")
   |> Array.map (fun s ->
      let index , beforeValue =
         let v = s.Split("|")
         v[0] |> int, v[1] |> int
      index, Before beforeValue
   )
   |> List.ofArray
   |> OrderL

let parsePrintL (s:string) =
   s.Split("\n")
   |> Array.map (fun s ->
         s.Split(",")
         |> Array.map int
         |> Array.toList
      )
   |> Array.toList
   |> PrintL


let determinePrintOrder (printL : List<int>) (rules : List<int * Before>) =
   printL
   |> List.mapi (fun i current ->
      let currentRules = rules |> List.filter (fun (index, _) -> index = current)
      let notCorrect =
         currentRules
         |> List.map (fun (_, Before rule) ->
            let isNotInList = printL |> List.filter (fun p -> p = rule) |> List.isEmpty
            if isNotInList then (current, true)
            else
               let index = printL |> List.findIndex (fun p -> p = rule)
               (current, i < index)
            )
         |> List.contains (current, false)
      (current, not notCorrect)
   )



let rec correctPrintOrder (printL : array<int>) (rules : List<int * Before>) =
   let fixOrdering =
      do
         determinePrintOrder (printL |> List.ofArray) rules
         |> List.toArray
         |> Array.mapi ( fun currentIndex (i, correct) ->
               if correct then ()
               else
                  printL
                  |> fun x -> x[currentIndex] <- x[currentIndex - 1] ; x
                  |> fun x -> x[currentIndex - 1] <- i
            )
         |> ignore

   fixOrdering

   let isCorrect =
      determinePrintOrder (printL |> List.ofArray) rules
      |> List.filter (fun (_,b) -> b = false)
      |> List.isEmpty

   if isCorrect then printL
   else correctPrintOrder printL rules


let resultPart1 (inputStr : string) =
   let textA = inputStr.Split("\n\n")

   let orderL = parseOrderingL textA[0] |> function OrderL list -> list
   let printL = parsePrintL textA[1] |> function PrintL list -> list

   printL
   |> List.map (fun l -> determinePrintOrder l orderL)
   |> List.filter (fun l -> l |> List.filter (fun (_,b) -> b = false) |> List.isEmpty)
   |> List.map (fun l -> l |> List.map fst)
   |> List.map (fun l ->
      let centerD = (l.Length |> float) / (2.0 |> float)
      let middle = Math.Floor(centerD |> decimal) |> int
      l[middle]
   )
   |> List.sum
   |> fun x -> printfn $"Part 1 : {x}"

let resultPart2 (inputStr : string) =
   let textA = inputStr.Split("\n\n")

   let orderL = parseOrderingL textA[0] |> function OrderL list -> list
   let printL = parsePrintL textA[1] |> function PrintL list -> list

   printL
   |> List.map (fun l -> determinePrintOrder l orderL)
   |> List.filter (fun l -> l |> List.filter (fun (_,b) -> b = false) |> List.isEmpty |> not)
   |> List.map (fun l ->
      l
      |> List.map fst
      |> fun l -> correctPrintOrder (l |> List.toArray) orderL
      )
   |> List.map (fun l ->
      let centerD = (l.Length |> float) / (2.0 |> float)
      let middle = Math.Floor(centerD |> decimal) |> int
      l[middle]
   )
   |> List.sum
   |> fun x -> printfn $"Part 2 : {x}"



resultPart1 testInput
resultPart1 inputL

resultPart2 testInput
resultPart2 inputL