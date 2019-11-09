module AOC2015.Day5_2
open System

let checkTwice (input :string) =
    let pairs = input |> Seq.pairwise |> Seq.mapi (fun i pair -> (pair, i))
    let map = pairs |> Map
    let checkPair (pair, i) =
        let lastPairI = map.Item pair
        if (lastPairI = i) then
            false
        else
            let (p1, p2) = pair
            p1 <> p2 || i + 1 <> lastPairI
            
    pairs |> Seq.exists checkPair

let checkBetween (input : string) =
    let rec check (array :array<'T>) i =
        if (i = array.Length - 2) then false else array.[i] = array.[i + 2] || check array (i + 1)
    input.Length > 2 && check (input.ToCharArray()) 0

let solve (input : string) : bool =
    checkTwice input
    && checkBetween input   

let solveRealInput =
    let result = System.IO.File.ReadAllLines "..\..\..\Inputs\Day5.txt" |> Array.fold (fun acc l -> acc + Convert.ToInt32(solve l)) 0 |> string
    System.IO.File.WriteAllText ("..\..\..\Outputs\Day5_2.txt", result)

let ``do`` =
    Checker.check true (checkTwice "xyxy")
    Checker.check true (checkTwice "aabcdefgaa")
    Checker.check false (checkTwice "aaa")
    
    Checker.check true (checkTwice "qjhvhtzxzqqjkmpb")
//    Checker.check true (checkBetween "qjhvhtzxzqqjkmpb")
//    Checker.check true (solve "qjhvhtzxzqqjkmpb")
    
    Checker.check true (checkTwice "xxyxx")
//    Checker.check true (checkBetween "xxyxx")
//    Checker.check true (solve "xxyxx")
    
    Checker.check true (checkTwice "uurcxstgmygtbstg")
//    Checker.check false (checkBetween "uurcxstgmygtbstg")
//    Checker.check false (solve "uurcxstgmygtbstg")
    
    Checker.check false (checkTwice "ieodomkazucvgmuy")
//    Checker.check true (checkBetween "ieodomkazucvgmuy")
//    Checker.check false (solve "ieodomkazucvgmuy")
//    solveRealInput