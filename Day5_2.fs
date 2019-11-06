module AOC2015.Day5_2
//open System
//
//let compareWithNext (fn : 'T -> 'T -> bool) (input : seq<'T>)  =
//    let temp = [|Unchecked.defaultof<'T>|]
//    Seq.exists2 fn (Seq.append temp input) (Seq.append input temp)
//
//let move (x, y) c =
//    let result = 
//        match c with
//        | '^' -> (x, y - 1)
//        | '<' -> (x - 1, y)
//        | '>' -> (x + 1, y)
//        | 'v' -> (x, y + 1)
//        | _ -> (x, y)
//    (result, result)
//
//let checkTwice input =
//    
//
//let checkForbiddenPair c1 c2 = Set.contains (c1, c2) forbiddenSubStrings
//let checkForbidden input = Seq.exists2 checkForbiddenPair (" " + input) (input + " ") |> not
//
//let solve (input : string) : bool =
//    checkTwice input &&
//    checkBetween input    
//
//let solveRealInput =
//    let result = System.IO.File.ReadAllLines "..\..\..\Inputs\Day5.txt" |> Array.fold (fun acc l -> acc + Convert.ToInt32(solve l)) 0 |> string
//    System.IO.File.WriteAllText ("..\..\..\Outputs\Day5_2.txt", result)
//
//let ``do`` =
//    Checker.check true (checkTwice "qjhvhtzxzqqjkmpb")
//    Checker.check true (checkBetween "qjhvhtzxzqqjkmpb")
//    Checker.check true (solve "qjhvhtzxzqqjkmpb")
//    
//    Checker.check true (checkTwice "xxyxx")
//    Checker.check true (checkBetween "xxyxx")
//    Checker.check true (solve "xxyxx")
//    
//    Checker.check true (checkTwice "uurcxstgmygtbstg")
//    Checker.check false (checkBetween "uurcxstgmygtbstg")
//    Checker.check false (solve "uurcxstgmygtbstg")
//    
//    Checker.check false (checkTwice "ieodomkazucvgmuy")
//    Checker.check true (checkBetween "ieodomkazucvgmuy")
//    Checker.check false (solve "ieodomkazucvgmuy")
//    solveRealInput