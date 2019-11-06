module AOC2015.Day3_1

let move (x, y) c =
    let result = 
        match c with
        | '^' -> (x, y - 1)
        | '<' -> (x - 1, y)
        | '>' -> (x + 1, y)
        | 'v' -> (x, y + 1)
        | _ -> (x, y)
    (result, result)

let solve (input : string) : int = Seq.mapFold move (0, 0) input |> fst |> Seq.append (seq [(0, 0)]) |> Seq.distinct |> Seq.length   

let solveRealInput =
    let result = System.IO.File.ReadAllText "..\..\..\Inputs\Day3.txt" |> solve |> string
    System.IO.File.WriteAllText ("..\..\..\Outputs\Day3_1.txt", result)

let ``do`` =
    Checker.check 2 (solve ">")
    Checker.check 4 (solve "^>v<")
    Checker.check 2 (solve "^v^v^v^v^v")
    solveRealInput