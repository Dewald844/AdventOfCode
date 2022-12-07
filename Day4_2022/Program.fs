
type SectionRange = { start: int; end': int }

let parseSectionRange (input: string) =
    let parts = input.Split '-'
    { start = int parts.[0]; end' = int parts.[1] }

let contains (range1: SectionRange) (range2: SectionRange) =
    range1.start <= range2.start && range1.end' >= range2.end'

let overlaps (range1: SectionRange) (range2: SectionRange) =
    range1.end' >= range2.start && range1.start <= range2.end'

let inputL () =
   System.IO.File.ReadLines("../../../input.txt") |> List.ofSeq

let part_1 () =
    let result =
        inputL ()
        |> List.map (fun s ->
            let ranges = s.Split ',' |> Array.map parseSectionRange
            contains ranges.[0] ranges.[1] || contains ranges.[1] ranges.[0]
        )
        |> List.filter id
        |> List.length

    printfn $"There are %d{result} pairs where one range fully contains the other."

let part_2 () =
    let result =
        inputL ()
        |> List.map (fun s ->
            let ranges = s.Split ',' |> Array.map parseSectionRange
            overlaps ranges.[0] ranges.[1]
        )
        |> List.filter id
        |> List.length

    printfn $"There are %d{result} pairs where they fully overlaps each other."

part_1 ()
part_2 ()
