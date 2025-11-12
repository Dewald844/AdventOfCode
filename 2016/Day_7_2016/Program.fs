open System

let testL = [
   "abba[mnop]qrst" // Pass
   "abcd[bddb]xyyx" // Fail
   "aaaa[qwer]tyui" // Fail
   "ioxxoj[asdfgh]zxcvbn" // Pass
   "aaab[qwer]baui" // Fail
   "aaab[qwer]bauiioxxoj[asdfgh]zxcvbnabcd[bddb]xyyxabba[mnop]qrst" // Pass
]

let inputL =
    (System.IO.File.ReadAllText "./input.txt").Trim().Split("\n")
    |> List.ofArray
    
let rec checkTLS (chars : List<char>) =
   if chars.Length >= 4 then
      match chars with
      | _head::tail ->
         let nextFourChars = chars[0..3]
         if (
            nextFourChars[0] <> nextFourChars[1] &&
            nextFourChars[1] = nextFourChars[2] &&
            nextFourChars[0] = nextFourChars[3]
         ) then
            true
         else 
            checkTLS tail
      | _ -> false
         
   else
      false

let hasTLSSupport (input : string) : bool =
   input.ToCharArray()
   |> List.ofArray
   |> checkTLS
   
let splitBrackets (input : string) =
   let openSplit = String.Join("_", input.Split('['))
   let closeSplit = String.Join("_", openSplit.Split(']'))
   let outside = 
      closeSplit.Split("_")
      |> Array.indexed
      |> Array.filter (fun (i, _) -> i % 2 = 0)
      |> Array.map snd
      |> List.ofArray
      
   let inside = 
      closeSplit.Split("_")
      |> Array.indexed
      |> Array.filter (fun (i, _) -> i % 2 <> 0)
      |> Array.map snd
      |> List.ofArray
   
   (outside, inside)

let partOne (inputL : List<String>) =
   inputL
   |> List.map splitBrackets
   |> List.filter (fun (outside, inside) ->
      (outside |> List.filter hasTLSSupport |> List.length) > 0 &&
      (inside |> List.filter hasTLSSupport |> List.length) = 0
   )
   |> List.length
   |> fun i -> printf $"Part 1 : %i{i}\n"

[<EntryPoint>]
let main _ =
   partOne(testL)
   partOne(inputL)
   0
   