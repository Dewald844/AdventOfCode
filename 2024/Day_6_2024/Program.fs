let testInput = """....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."""

let inputString = System.IO.File.ReadAllText "./input.txt"

type Direction =
   | Up
   | Right
   | Down
   | Left

   with
      member this.next =
         match this with
         | Up    -> Right
         | Right -> Down
         | Down  -> Left
         | Left  -> Up

type Position = { x : int; y : int }
   with
      member this.Move (direction : Direction) =
         match direction with
         | Up    -> { this with y = this.y - 1 }
         | Right -> { this with x = this.x + 1 }
         | Down  -> { this with y = this.y + 1 }
         | Left  -> { this with x = this.x - 1 }

type MatrixExit = Loop | OutOfBounds

let parseInput (input : string) =
   input.Split("\n")
   |> Array.map (fun s -> s.ToCharArray())

let findStartingPosition (matrix : array<array<char>>) =
   let mutable posX = 0
   let mutable posY = 0
   for y in 0..matrix.Length - 1 do
      for x in 0..matrix[y].Length - 1 do
         if matrix[y][x] = '^' then
            posX <- x
            posY <- y
   { x = posX; y = posY }

let rec findPath (matrix : array<array<char>>) (position : Position) (direction : Direction) =
   let newPosition = position.Move direction
   if newPosition.x < 0 || newPosition.x >= matrix[0].Length || newPosition.y < 0 || newPosition.y >= matrix.Length then
      []
   else
      match matrix[newPosition.y][newPosition.x] with
      | '#' ->
         let nextPosition = position.Move direction.next
         nextPosition :: findPath matrix nextPosition direction.next
      | _ -> newPosition :: findPath matrix newPosition direction

open System.Collections.Generic

let rec checkForLoop (matrix : array<array<char>>) (position : Position) (direction : Direction) (visited : HashSet<Position * Direction>) =
   let newPosition = position.Move direction
   if newPosition.x < 0 || newPosition.x >= matrix[0].Length || newPosition.y < 0 || newPosition.y >= matrix.Length then
      OutOfBounds
   else
      match matrix[newPosition.y][newPosition.x] with
      | '#' ->
         let nextPosition = position.Move direction.next
         if visited.Contains((nextPosition, direction.next)) then
            Loop
         else
            visited.Add((nextPosition, direction.next)) |> ignore
            checkForLoop matrix nextPosition direction.next visited
      | _ ->
         visited.Add((newPosition, direction)) |> ignore
         checkForLoop matrix newPosition direction visited

let placeObstacle (matrix : array<array<char>>) (position : Position) =
   matrix
   |> Array.mapi (fun y a ->
      a
      |> Array.mapi (fun x v ->
         if x = position.x && y = position.y then '#'
         else v
   ))

let calculateNextObstaclePos (currentPos : Position) (matrix : array<array<char>>) =
   if currentPos.x < matrix[0].Length - 1 then
      { currentPos with x = currentPos.x + 1 }
   else
      { currentPos with x = 0; y = currentPos.y + 1 }

let rec placeAndFindLoops (obstaclePos : Position) (currentPos : Position) (direction : Direction) (matrix : array<array<char>>) loopL =
   let newMatrix = placeObstacle matrix obstaclePos
   let nextObstaclePos = calculateNextObstaclePos obstaclePos matrix
   if nextObstaclePos.y < matrix.Length then
      let loop = checkForLoop newMatrix currentPos direction (HashSet<Position * Direction>([currentPos, direction]))
      match loop with
      | Loop ->
         let newLoopL = loopL @ [obstaclePos]
         placeAndFindLoops nextObstaclePos currentPos direction matrix newLoopL
      | OutOfBounds ->
         placeAndFindLoops nextObstaclePos currentPos direction matrix loopL
   else
      loopL

let resultPart1 (input : string) =
   let matrix = parseInput input
   let startingPosition = findStartingPosition matrix
   let path = findPath matrix startingPosition Up
   printfn $"Part 1 : {path |> List.append [startingPosition] |> List.distinctBy (fun p -> p.y, p.x) |> List.length}"

let resultPart2 (input : string) =
   let matrix = parseInput input
   let startingPosition = findStartingPosition matrix
   let loopL = placeAndFindLoops {x = 0; y = 0} startingPosition Direction.Up matrix []
   printfn $"Part 2 : %A{loopL |> List.length}"

resultPart1 testInput
resultPart1 inputString

resultPart2 testInput
resultPart2 inputString