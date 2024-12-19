open System.Security.Cryptography
open System.Text

let computeHash (input: string) =
   let md5 = MD5.Create()
   let inputBytes = Encoding.ASCII.GetBytes input
   let hashBytes = md5.ComputeHash inputBytes
   System.BitConverter.ToString(hashBytes).Replace("-", "").ToLower()

let rec findPasswordPart1 (input: string) (index: int) (password: string) =
   if String.length password = 8 then password
   else
      let hash = computeHash (input + string index)
      if hash.StartsWith "00000" then
         let newPassword = $"{password}{hash[5]}"
         findPasswordPart1 input (index + 1) newPassword
      else
         findPasswordPart1 input (index + 1) password

let rec findPasswordPart2 (input: string) (index: int) (password: string) =
   if password.Contains("_") then
      let hash = computeHash (input + string index)
      if hash.StartsWith "00000" then
         let position = hash[5]
         let value = hash[6]
         if position >= '0' && position <= '7' then
            let position = int (string position)
            let newPassword =
               if password[position] = '_' then
                  let newPassword = password.ToCharArray()
                  newPassword[position] <- value
                  new string(newPassword)
               else
                  password
            findPasswordPart2 input (index + 1) newPassword
         else
            findPasswordPart2 input (index + 1) password
      else
         findPasswordPart2 input (index + 1) password
   else password

let input = "abbhdwsy"
let testInput = "abc"

let part1Test = findPasswordPart1 testInput 0 ""
let part1 = findPasswordPart1 input 0 ""
let part2Test = findPasswordPart2 testInput 0 "________"
let part2 = findPasswordPart2 input 0 "________"

printfn $"Result Test 1 : {part1Test}"
printfn $"Result Part 1 : {part1}"
printfn $"Result Test 2 : {part2Test}"
printfn $"Result Part 2 : {part2}"