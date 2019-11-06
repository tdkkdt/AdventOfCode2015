module AOC2015.Day2_1

let split (size : string) : (int * int * int) =
    let array = size.Split [|'x'|] |> Array.map int
    (array.[0], array.[1], array.[2]) 

let solve size : int =
    let (l, w, h) = split size
    let sur1  = l * w
    let sur2 = l * h
    let sur3 = w * h
    sur1 * 2+  sur2 * 2 + sur3 * 2 + min sur1 (min sur2 sur3)

let solveRealInput =
    let result = System.IO.File.ReadAllLines "..\..\..\Inputs\Day2.txt" |> Array.fold (fun acc line -> acc + solve line) 0 |> string
    System.IO.File.WriteAllText ("..\..\..\Outputs\Day2_1.txt", result)

let ``do`` =
    Checker.check 58 (solve "2x3x4")
    Checker.check 43 (solve "1x1x10")
    solveRealInput