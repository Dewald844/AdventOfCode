// --- Day 4: Scratchcards ---

let inputL = System.IO.File.ReadLines("../../../input.txt") |> List.ofSeq

type Card = {
   CardNumber : int
   WinningNumbers : int list
   ScratchNumbers : int list
} with
   member this.Winnings =
      this.ScratchNumbers
      |> List.filter (fun x -> this.WinningNumbers |> List.contains x)
      |> List.length


let test = [
   "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
   "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
   "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
   "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
   "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
   "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
]

let mapStringInputToNumbers (input : array<string>) : List<int> =
   input
   |> Array.map (fun x -> if x = "" then None else Some(int x))
   |> Array.choose id
   |> Array.map int
   |> Array.toList

let parseInputString (s : string) =
   let cardNumber = ((s.Split(":")[0]).Split("Card")[1]).Trim() |> int
   let winningNumbers =

      ((s.Split(":")[1]).Split("|")[0]).Split(" ")
      |> mapStringInputToNumbers

   let scratchNumbers =
      ((s.Split(":")[1]).Split("|")[1]).Split(" ")
      |> mapStringInputToNumbers

   { CardNumber = cardNumber; WinningNumbers = winningNumbers; ScratchNumbers = scratchNumbers }


// =========== Part 1 ===========

let calculateWinningNumbers (card : Card) =

   let mutable score = 0.0

   do
      card.ScratchNumbers
      |> List.filter (fun x -> card.WinningNumbers |> List.contains x)
      |> List.iter (fun _ ->
         if score = 0.0 then score <- 1.0
         else score <- score * 2.0)

   score

let cards = inputL |> List.map parseInputString

let part1 =
   cards
   |> List.map calculateWinningNumbers
   |> List.sum

// =========== Part 2 ===========

let evolveState (state : (int * int) array) (card : Card) =

   let toUpdate =
      if card.Winnings = 0 then
         None
      else
         Some(card.CardNumber + 1, card.CardNumber + card.Winnings)

   let currentCardCount =
      state
      |> Array.find (fun row -> row |> fst = card.CardNumber)
      |> fun x -> x |> snd

   let state =
      state
      |> Array.map (fun row ->
         match toUpdate with
         | None -> row
         | Some(min, max) ->
            if row |> fst >= min && row |> fst <= max then
               (row |> fst, (row |> snd) + currentCardCount)
            else
               row)

   state

let calculateNumberOfScratchCards cards =

    let state = cards |> Array.map (fun c -> (c.CardNumber, 1))

    (state, cards)
    ||> Array.fold evolveState
    |> Array.sumBy (fun row -> row |> snd)

let part2 =
   inputL
   |> List.map parseInputString
   |> List.toArray
   |> calculateNumberOfScratchCards

printf  "+++++++++++ Part 1 +++++++++++++++++"
printf $"\nPart 1 : %A{part1}"
printf  "\n+++++++++++ Part 2 +++++++++++++++++"
printf $"\nPart 2 : %A{part2}"

