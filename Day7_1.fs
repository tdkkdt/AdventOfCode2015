module AOC2015.Day7_1

open System

type Gate = 
    | Num of a : uint16
    | Assignment of a : string
    | And of a : string * b : string
    | Or of a : string * b : string
    | LShift of a : string * b : string
    | RShift of a : string * b : string
    | Not of a : string

let rShift (a : uint16) (b : uint16) : uint16 = a >>> (int b)
let lShift (a : uint16) (b : uint16) : uint16 = a <<< (int b)

let rec calculate2 (gatesStack : string list) (map : Map<string, Gate>) : Map<string, Gate> =
    match gatesStack with
        | [] -> map
        | head::tail ->
            let gate = map |> Map.find head            
            let processLastArg argName fn =
                let arg = map |> Map.find argName
                match arg with
                | Num a -> (tail, map.Add(head, (fn a) |> Num))
                | _ -> (argName::gatesStack, map)
            let processArgs arg1Name arg2Name fn =
                let arg1 = map |> Map.find arg1Name
                match arg1 with
                | Num a -> processLastArg arg2Name (fn a)
                | _ -> (arg1Name::gatesStack, map)
            let (newStack, newMap) =
                match gate with
                | Num _ -> (tail, map)
                | Assignment arg1 -> processLastArg arg1 id
                | And (arg1, arg2) -> processArgs arg1 arg2 (&&&)
                | Or (arg1, arg2) -> processArgs arg1 arg2 (|||)
                | LShift (arg1, arg2) -> processArgs arg1 arg2 lShift
                | RShift (arg1, arg2) -> processArgs arg1 arg2 rShift
                | Not arg1 -> processLastArg arg1 (~~~)
            calculate2 newStack newMap
            
    
let calculate (name : string) (map : Map<string, Gate>) : uint16 =
    let newMap = calculate2 [name] map
    let gate = newMap |> Map.find name
    match gate with
    | Num a -> a
    | _ -> 0us   

let parseLine (line : string) (map : Map<string, Gate>) : Map<string, Gate> =
    let parseGate (arg : string) (map1 : Map<string, Gate>) : Map<string, Gate> =
        match System.UInt16.TryParse arg with
            | true, num -> map1.Add (arg, (Num num)) 
            | _ -> map1
            
    let array = line.Split([|"->"; " "|],  StringSplitOptions.RemoveEmptyEntries)
    if array.Length = 2 then
        map |> Map.add array.[1] (Assignment array.[0]) |> parseGate array.[0]
    elif array.Length = 3 then
        map |> Map.add array.[2] (Not array.[1]) |> parseGate array.[1]
    else
        let gate = match array.[1] with
                    | "AND" -> And
                    | "OR" -> Or
                    | "LSHIFT" -> LShift
                    | "RSHIFT" -> RShift
        map |> Map.add array.[3] ((array.[0], array.[2]) |> gate) |> parseGate array.[0] |> parseGate array.[2] 
    
let ``do`` =
    let map = Map.ofList [
        ("123", Num 123us);
        ("456", Num 456us);
        ("x", Assignment "123");
        ("y", Assignment "456");
        ("d", And ("x", "y"));
        ("e", Or ("x", "y"));
        ("f", LShift ( "x",  "2"));
        ("2", Num 2us);
        ("g", RShift ( "y", "2"));
        ("h", Not "x");
        ("i", Not  "y");
    ]
    
    do Checker.check (72us) (calculate "d" map)
    do Checker.check (507us) (calculate "e" map)
    do Checker.check (492us) (calculate "f" map)
    do Checker.check (114us) (calculate "g" map)
    do Checker.check (65412us) (calculate "h" map)
    do Checker.check (65079us) (calculate "i" map)
    do Checker.check (123us) (calculate "x" map)
    do Checker.check (456us) (calculate "y" map)
    
    let parsedMap = Map.empty
                    |> parseLine "123 -> x" 
                    |> parseLine "456 -> y"
                    |> parseLine "x AND y -> d"
                    |> parseLine "x OR y -> e"
                    |> parseLine "x LSHIFT 2 -> f"
                    |> parseLine "y RSHIFT 2 -> g"
                    |> parseLine "NOT x -> h"
                    |> parseLine "NOT y -> i"
    
    do Checker.check map parsedMap
        
    let result = System.IO.File.ReadAllLines "..\..\..\Inputs\Day7.txt" |> Array.fold (fun acc l -> parseLine l acc) Map.empty |> (calculate "a") |> string
    System.IO.File.WriteAllText ("..\..\..\Outputs\Day7_1.txt", result)
