open System

let testInput1 = [|
    "aaaaa-bbb-z-y-x-123[abxyz]"
    "a-b-c-d-e-f-g-h-987[abcde"
    "not-a-real-room-404[oarel]"
    "totally-real-room-200[decoy]"
|]

let inputArray = System.IO.File.ReadLines "./input.txt" |> Seq.toArray

type Room = {
    Id : int
    CheckSum : array<char>
    EncryptedName : string
}

let parseLine (s : string) =
    let parts = s.Split([|'-'; '['; ']'|], System.StringSplitOptions.RemoveEmptyEntries)
    let id = int parts.[parts.Length - 2]
    let checkSum = parts.[parts.Length - 1].ToCharArray()
    let name = parts.[0..parts.Length - 3] |> String.concat("-")
    {
        Id = id
        CheckSum = checkSum
        EncryptedName = name
    }
    
let isValid (room : Room) : bool =
    let frequency =
        room.EncryptedName.Replace("-", "")
        |> Seq.groupBy id
        |> Seq.map (fun (char, occurrences) -> (char, Seq.length occurrences))
        |> Seq.sortBy (fun (char, count) -> (-count, char))
        |> Seq.map fst
        |> Seq.take 5
        |> Seq.toArray

    frequency = room.CheckSum
    
let shiftChar (c: char) (shift: int) : char =
    let base' = int 'a'
    let offset = (int c - base' + shift) % 26
    char (base' + offset)

let resultPart1 (input : array<string>) =
    input
    |> Array.map parseLine
    |> Array.filter isValid
    |> Array.sumBy _.Id
    
let resultPart2 (input : array<string>) =
    input
    |> Array.map parseLine
    |> Array.filter isValid
    |> Array.map (fun r ->
        r.EncryptedName.Split('-')
        |> Array.map (fun s ->
            s.ToCharArray()
            |> Array.map (fun c -> shiftChar c r.Id)
            |> fun x -> printfn $"%A{x}"; new String(x)
        )
        |> String.concat(" "), r.Id
    )
    |> Array.filter (fun (s, id) -> s.Contains("north"))
    
    
printfn $"Test part 1 : {resultPart1 testInput1}"
printfn $"Part 1 : {resultPart1 inputArray}"

printfn $"Part 2 :%A{resultPart2 inputArray}"