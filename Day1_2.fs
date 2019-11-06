module AOC2015.Day1_2

let rec findPosition (input : seq<char>) (index : int) (acc : int) : int =
    let newAcc = if Seq.head input = '(' then acc + 1 else acc - 1
    if newAcc = -1 then
        index
     else findPosition (Seq.tail input) (index + 1) newAcc

let solve (input : string) : int = findPosition input 1 0
    
let ``do`` =
    Checker.check 1 (solve ")")
    Checker.check 5 (solve "()())")
    let result = System.IO.File.ReadAllText "..\..\..\Inputs\Day1.txt" |> solve |> string
    System.IO.File.WriteAllText ("..\..\..\Outputs\Day1_2.txt", result)
