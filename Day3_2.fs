module AOC2015.Day3_2

let move ((x1, y1), (x2, y2)) c =
    let result = 
        match c with
        | '^' -> (x1, y1 - 1)
        | '<' -> (x1 - 1, y1)
        | '>' -> (x1 + 1, y1)
        | 'v' -> (x1, y1 + 1)
        | _ -> (x1, y1)
    (result, ((x2, y2), result))

let solve (input : string) : int = Seq.mapFold move ((0, 0), (0, 0)) input |> fst |> Seq.append (seq [(0, 0)]) |> Seq.distinct |> Seq.length   

let solveRealInput =
    let result = System.IO.File.ReadAllText "..\..\..\Inputs\Day3.txt" |> solve |> string
    System.IO.File.WriteAllText ("..\..\..\Outputs\Day3_2.txt", result)

let ``do`` =
    Checker.check 3 (solve "^v")
    Checker.check 3 (solve "^>v<")
    Checker.check 11 (solve "^v^v^v^v^v")
    solveRealInput