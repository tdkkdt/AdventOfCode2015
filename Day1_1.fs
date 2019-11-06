module AOC2015.Day1
open AOC2015
open AOC2015

let solve (input : string) : int = Seq.fold (fun acc c -> if (c=')') then acc - 1 else acc + 1) 0 input

let solveRealInput =
    let result = System.IO.File.ReadAllText "..\..\..\Inputs\Day1.txt" |> solve |> string
    System.IO.File.WriteAllText ("..\..\..\Outputs\Day1.txt", result)

let ``do`` =
    Checker.check 0 (solve "(())")
    Checker.check 0 (solve "()()")
    Checker.check 3 (solve "(((")
    Checker.check 3 (solve "(()(()(")
    Checker.check 3 (solve "))(((((")
    Checker.check -1 (solve "())")
    Checker.check -1 (solve "))(")
    Checker.check -3 (solve ")))")
    Checker.check -3 (solve ")())())")
    solveRealInput