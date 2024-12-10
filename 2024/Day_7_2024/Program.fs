
let testInput = """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"""

let inputString = System.IO.File.ReadAllText "./input.txt"

type Op = Add | Multiply | Concat

let parseLine (line: string) =
    let parts = line.Split(": ")
    let testValue = int64 parts[0]
    let numbers = parts[1].Split(" ") |> Array.map int64 |> Array.toList
    testValue, numbers

let applyOp (a: int64) (b: int64) (op: Op) =
    match op with
    | Add -> a + b
    | Multiply -> a * b
    | Concat -> int64 $"{string a}{string b}"

let rec evaluate (numbers: List<int64>) (ops: Op list) =
    match numbers, ops with
    | [x], [] -> x
    | x::xs, op::ops' -> evaluate (List.append [applyOp x xs[0] op] (List.skip 1 xs)) ops'
    | _ -> failwith "Invalid input"

let rec generateOps (n: int64) =
    if n = 0 then [[]]
    else
        let rest = generateOps (n - 1L)
        List.collect (fun ops -> [Add::ops; Multiply::ops; Concat::ops]) rest

let isEquationTrue (testValue: int64) (numbers: List<int64>) =
    let opsCombinations = generateOps ((List.length numbers - 1) |> int64)
    opsCombinations
    |> List.exists (fun ops -> evaluate numbers ops = testValue)

let calculateTotalCalibrationResult (input: string) =
    input.Split("\n")
    |> Array.map parseLine
    |> Array.filter (fun (testValue, numbers) -> isEquationTrue testValue numbers)
    |> Array.map fst
    |> Array.map int64
    |> Array.sum

let result = calculateTotalCalibrationResult inputString
printfn "Total calibration result: %A" result