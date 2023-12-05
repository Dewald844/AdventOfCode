
open System.Text.RegularExpressions

let input = System.IO.File.ReadLines("../../../input.txt") |> List.ofSeq

let test =
    """
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3
move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""
        .Split("\n")
    |> Seq.skip 1
    |> List.ofSeq

type Stack = List<char>
type Stacks = List<Stack>

type Instruction = {Quantity : int; Source : int; Destination : int}

let instructionFromString s =

   let regexFn = Regex("move (\d+) from (\d) to (\d)", RegexOptions.Compiled)
   let regexMatch = regexFn.Match(s)
   let qty = regexMatch.Groups.[1].Value |> int
   let src = regexMatch.Groups.[2].Value |> int
   let des = regexMatch.Groups.[3].Value |> int

   {Quantity = qty; Source = src; Destination = des}

let parseRow s  =
   s
   |> Seq.chunkBySize 4
   |> Seq.map (function
      | [| ' '; ' '; ' '; ' ' |] -> None
      | [| ' '; ' '; ' ' |]      -> None
      | [| '['; c; ']' |]        -> Some c
      | [| '['; c; ']'; ' ' |]   -> Some c
   //   | _ -> None
   )
   |> List.ofSeq

let parse lines =

   let initial =
      lines
      |> List.takeWhile ((<>) "")
      |> List.rev
      |> List.skip 1
      |> List.rev

   let rows = initial |> List.map parseRow

   let stacks =
      [ for i in 0 .. (rows.[0] |> Seq.length) - 1 ->
         rows
         |> List.map (fun r -> r[i])
         |> List.choose id ]

   let instructions =
      lines
      |> List.skipWhile ((<>) "")
      |> List.skip 1
      |> List.map instructionFromString

   stacks, instructions

let apply (stackL : Stacks) (instruction : Instruction) =

   let toMove, toStay =
      stackL[instruction.Source - 1]
      |> List.splitAt instruction.Quantity
   let newDestination =
      toMove @ stackL[instruction.Destination - 1]

   stackL
   |> List.mapi (fun i element ->
      if i = (instruction.Source - 1) then toStay
      elif i = (instruction.Destination - 1) then newDestination
      else element
   )

let run input =
   let stacks, instructions = parse input
   let after = instructions |> List.fold apply stacks

   after
   |> List.map List.head
   |> List.map string
   |> String.concat ""

//printf $"Test{test |> run}"
printf $"Answer Part 1 : {run input}"