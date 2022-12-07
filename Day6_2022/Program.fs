open System
let solve (input : string) =
    let rec loop pos s =
        if pos < 3
        then loop (pos + 1) (input[pos] :: s)
        else if s |> Seq.distinct |> Seq.length = 4 then pos - 3
        else loop (pos + 1) (input[pos] :: (List.tail s))
    loop 0 []

let input = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
printfn $"First marker after character %d{solve input}"
