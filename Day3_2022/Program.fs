// For more information see https://aka.ms/fsharp-console-apps
open System.IO

let inputL () =
   File.ReadLines("../../../input.txt")

let alphabet =  // There must be easier way to do this but all that i can think of now
   [
      'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z'
      'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z'
   ]

let charScore c = (alphabet |> List.findIndex (fun char -> char = c)) + 1

let findCommon s =
   s
   |> Seq.map Set.ofSeq
   |> Seq.reduce Set.intersect
   |> Set.toSeq

let part1 =
   inputL ()
   |> Seq.collect (Seq.splitInto 2 >> findCommon)
   |> Seq.sumBy charScore

let part2 =
   inputL ()
   |> Seq.chunkBySize 3
   |> Seq.collect findCommon
   |> Seq.sumBy charScore

printf $"Sum of part 1 data : {part1}\n"
printf $"Sum of part 2 data : {part2}\n"