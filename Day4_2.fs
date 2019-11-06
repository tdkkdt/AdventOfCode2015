module AOC2015.Day4_2
open System.Security.Cryptography
open System.Text

let calcHash (input : string) (i : int) =
     (input + string i) |> Encoding.UTF8.GetBytes |> MD5.Create().ComputeHash |> Array.map (fun (x) -> System.String.Format("{0:X2}", x)) |> String.concat ""

let rec solve2 (input : string) (i : int) : int =    
    let hash = calcHash input i
    if (hash.StartsWith "000000") then
        i
    else
        solve2 input (i + 1)

let solve (input : string) : int = solve2 input 1
    

let solveRealInput =
    let result = "ckczppom" |> solve |> string
    System.IO.File.WriteAllText ("..\..\..\Outputs\Day4_2.txt", result)

let ``do`` =
//    Checker.check 609043 (solve "abcdef")
//    Checker.check 1048970 (solve "pqrstuv")
    solveRealInput
    0