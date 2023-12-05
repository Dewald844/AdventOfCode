

open System.Threading.Tasks
(*
module Part1 =

  type Position = { Row: int; Column: int }
  type Number = { Value : int; IsMarked: bool }
  type Row = Row of List<Position * Number>
  type Board = Board of  List<Row>

  let input = System.IO.File.ReadLines "./input.txt" |> Seq.toList
  let testInput =
    """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

    22 13 17 11  0
     8  2 23  4 24
    21  9 14 16  7
     6 10  3 18  5
     1 12 20 15 19

     3 15  0  2 22
     9 18 13 17  5
    19  8  7 25 23
    20 11 10 24  4
    14 21 16 12  6

    14 21 17 24  4
    10 16 15  9 19
    18  8 23 26 20
    22 11 13  6  5
     2  0 12  3  7
    """.Split("\n")
    |> Array.map (fun x -> x.Trim())

  let rec parseBoards (boardLines: string []) =
    if boardLines |> Seq.isEmpty then
      []
    else
      let board =
          boardLines
          |> List.ofArray
          |> List.take 5
          |> List.indexed
          |> List.map ( fun (row, line) ->
              line.Split(" ")
              |> List.ofArray
              |> List.except [ "" ]
              |> List.mapi (fun index value -> index, int value)
              |> List.map ( fun (column, number) -> ({Row = row ; Column = column}, { Value = number; IsMarked = false })))
          |> List.concat
          |> List.chunkBySize 5
          |> List.map Row
          |> List.chunkBySize 5
          |> List.map Board

      let remainder = boardLines |> Array.skip (if boardLines |> Seq.length > 5 then 6 else 5)
      board @ (parseBoards remainder)

  let inputManipulation (text : string []) =
    let numbers =
      text.[0].Split(",")
      |> Array.map int
      |> Array.toList
    let boards = parseBoards (text |> Array.skip 2)
    numbers, boards

  let markBoardValue ((number : int) ,((position : Position) , (value : Number))) : Position * Number =
      if number = value.Value then
        (position,{ value with IsMarked = true })
      else (position, value)

  let isWinner (Board boardL) =
    // List of boards
    let rowsIsMarked =
      boardL
      |> List.map(fun (Row rowList) ->
          // List.of Rows
          rowList
          |> List.filter (fun (_pos, num) -> num.IsMarked )
          |> List.groupBy (fun (pos,row) -> pos.Row)
          |> List.map (fun (row, list) ->
              let numberOfMarkedInRow = list |> List.length
              if numberOfMarkedInRow = 5 then Some rowList else None ) )
      |> List.concat
      |> List.filter (fun x -> x |> Option.isSome)

    let columnIsMarked =
      boardL
      |> List.map(fun (Row rowList) ->
          // List.of Rows
          rowList
          |> List.filter (fun (_pos, num) -> num.IsMarked )
          |> List.groupBy (fun (pos,row) -> pos.Column)
          |> List.map (fun (row, list) ->
              let numberOfMarkedInRow = list |> List.length
              if numberOfMarkedInRow = 5 then Some rowList else None ) )
      |> List.concat
      |> List.filter (fun x -> x |> Option.isSome)
    if rowsIsMarked |> List.isEmpty || columnIsMarked |> List.isEmpty then None
    elif not (rowsIsMarked |> List.isEmpty) then Some rowsIsMarked
    elif not (columnIsMarked |> List.isEmpty) then Some columnIsMarked
    else None

  let rec bingo (numbers : List<int>, Board boardL) =
    let afterNumber =
      numbers
      |> List.map ( fun number ->
       boardL
       |> List.map (fun (Row x) -> x)
       |> List.concat
       |> List.map (fun rowAndPos -> markBoardValue (number, rowAndPos))
       |> List.chunkBySize 5
       |> List.map Row.Row
       |> List.map Board
       |> List.concat
       |> List.chunkBySize 5
       |> List.concat )
      |> id
    bingo numbers boardL

  let boards = parseBoards (testInput |> Array.skip 2)

  printf $"1st board : %A{boards |> List.head}\n"
  *)
  //printf $"%A{testInput}\n"
 // printf $"%A{(inputManipulation testInput |> List.head)}\n"