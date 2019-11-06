module AOC2015.Day5_1
open System

let compareWithNext (fn : 'T -> 'T -> bool) (input : seq<'T>)  =
    let temp = [|Unchecked.defaultof<'T>|]
    Seq.exists2 fn (Seq.append temp input) (Seq.append input temp)
    
let vowels = Set.ofList ['a' ; 'e' ; 'i' ; 'o' ; 'u']
let forbiddenSubStrings = Set.ofList [('a', 'b') ; ('c', 'd') ; ('p', 'q') ; ('x', 'y')]

let checkVowel acc c = acc + Convert.ToInt32(Set.contains c vowels)

let checkVowels input = (Seq.fold checkVowel 0 input) >= 3

let checkTwice input = compareWithNext (=) input

let checkForbiddenPair c1 c2 = Set.contains (c1, c2) forbiddenSubStrings
let checkForbidden input = compareWithNext checkForbiddenPair input |> not

let solve (input : string) : bool =
    checkVowels input &&
    checkTwice input &&
    checkForbidden input    

let solveRealInput =
    let result = System.IO.File.ReadAllLines "..\..\..\Inputs\Day5.txt" |> Array.fold (fun acc l -> acc + Convert.ToInt32(solve l)) 0 |> string
    System.IO.File.WriteAllText ("..\..\..\Outputs\Day5_1.txt", result)

let ``do`` =
    Checker.check true (checkVowels "ugknbfddgicrmopn")
    Checker.check true (checkTwice "ugknbfddgicrmopn")
    Checker.check true (checkForbidden "ugknbfddgicrmopn")
    Checker.check true (checkVowels "aaa")
    Checker.check true (checkTwice "aaa")
    Checker.check true (checkForbidden "aaa")
    Checker.check true (solve "ugknbfddgicrmopn")
    Checker.check true (solve "aaa")
    Checker.check false (solve "jchzalrnumimnmhp")
    Checker.check false (checkForbidden "haegwjzuvuyypxyu")
    Checker.check false (solve "haegwjzuvuyypxyu")
    Checker.check false (solve "dvszwmarrgswjxmb")
    solveRealInput