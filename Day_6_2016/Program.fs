let inputL =
    System.IO.File.ReadAllText "./input.txt"
    |> _.Trim() |> _.Split("\n")
    |> List.ofArray

let getMostCommon (map : Map<char, int>) =
    map
    |> Map.toList
    |> List.sortByDescending snd
    |> List.head
    |> fst
    
let getLeastCommon (map : Map<char, int>) =
    map
    |> Map.toList
    |> List.sortBy snd
    |> List.head
    |> fst

let buildCountMapL (l : List<string>) =
    l
    |> List.map (fun s -> s.Trim() |> _.ToCharArray() |> List.ofArray)
    |> List.transpose
    |> List.map (fun chars ->
        let mutable countMap : Map<char, int> = Map.empty
        chars
        |> List.iter (fun c ->
            match countMap |> Map.tryFind c with
            | Some i -> countMap <- countMap |> Map.add c (i + 1)
            | None -> countMap <- countMap |> Map.add c 1
        )
        countMap
    )

let part1 (l : List<string>) =
    buildCountMapL l
    |> List.map getMostCommon
    |> List.toArray
    |> System.String
    
let part2 (l : List<string>) =
    buildCountMapL l
    |> List.map getLeastCommon
    |> List.toArray
    |> System.String
    
[<EntryPoint>]
let main _args =
  printf $"Part 1 : %A{part1 inputL}\n"
  printf $"Part 2 : %A{part2 inputL}\n"
  0