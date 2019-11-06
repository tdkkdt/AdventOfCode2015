module AOC2015.Day2_2

let split (size : string) :int[] = size.Split [|'x'|] |> Array.map int |> Array.sort 

let solve size : int =
    let array = split size
    2 * array.[0] + 2 * array.[1] + array.[0] * array.[1] * array.[2]
    
let solveRealInput =
    let result = System.IO.File.ReadAllLines "..\..\..\Inputs\Day2.txt" |> Array.fold (fun acc line -> acc + solve line) 0 |> string
    System.IO.File.WriteAllText ("..\..\..\Outputs\Day2_2.txt", result)

let ``do`` =
    Checker.check 34 (solve "2x3x4")
    Checker.check 14 (solve "1x1x10")
    solveRealInput