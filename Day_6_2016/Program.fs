let inputL = List.ofArray ((System.IO.File.ReadAllText "./input.txt").Trim().Split("\n"))
 
let getMostCommon (map : Map<char, int>) = (Map.toList >> List.sortByDescending snd >> List.head >> fst) map
    
let getLeastCommon (map : Map<char, int>) = (Map.toList >> List.sortBy snd >> List.head >> fst) map
   
let rec countMap (map : Map<char, int>) (chars : List<char>) =
    match chars with
    | c::tail -> map |> Map.tryFind c |> Option.defaultValue 0 |> fun i -> countMap (map |> Map.add c (i+1)) tail 
    | [] -> map

let buildMap (l : List<string>) =
    l |> (List.map (_.Trim().ToCharArray() >> List.ofArray) >> List.transpose >> List.map (countMap Map.empty)) 

let buildString sortFn (l : List<string>) = (List.map sortFn >> List.toArray >> System.String) (buildMap l)

[<EntryPoint>]
let main _args =
    printf $"Part 1 : %A{buildString getMostCommon inputL}\nPart 2 : %A{buildString getLeastCommon inputL}"
    0