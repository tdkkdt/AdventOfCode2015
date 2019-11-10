module AOC2015.Day6_1
open System

type Rect = {
    Top : int;
    Left : int;
    Right : int;
    Bottom : int; 
} with
    member this.MiddlePoint : (int * int) = (this.Left + (this.Right - this.Left) / 2, this.Top + (this.Bottom - this.Top) / 2)
    member this.Split : (Rect * Rect * Rect * Rect) =
            let (midX, midY) = this.MiddlePoint
            let areaTopLeftRect = {this with Right = midX; Bottom = midY}
            let areaTopRightRect = {this with Left = midX + 1; Bottom = midY}
            let areaBottomLeftRect = {this with Right = midX; Top = midY + 1}
            let areaBottomRightRect = {this with Left = midX + 1; Top = midY + 1}
            (areaTopLeftRect, areaTopRightRect, areaBottomLeftRect, areaBottomRightRect)
        

type Area = {
    TopLeftArea : Area option;
    TopRightArea : Area option;
    BottomLeftArea : Area option;
    BottomRightArea : Area option;
    } with
    member this.HasNoChildren : bool = this.TopLeftArea.IsNone && this.TopRightArea.IsNone && this.BottomLeftArea.IsNone && this.BottomRightArea.IsNone
    static member CreateTurned () : Area = {TopLeftArea = None; TopRightArea = None; BottomLeftArea = None; BottomRightArea = None}
    
let ifThenElse (option : 'T option) (ifFn : 'T -> 'U option) (elseOpt : 'U option) : 'U option =
    match option with
    | Some a -> ifFn a
    | None -> elseOpt    

let SplitToIntersectionRects (rect : Rect) (midX : int, midY : int) : (Rect option * Rect option * Rect option * Rect option) =
    let topLeftRect = if rect.Left <= midX && rect.Top <= midY then Some ({rect with Bottom = min midY rect.Bottom; Right = min midX rect.Right}) else None
    let topRightRect = if rect.Right > midX && rect.Top <= midY then Some ({rect with Left = max (midX + 1) rect.Left; Bottom = min midY rect.Bottom}) else None
    let bottomLeftRect = if rect.Left <= midX && rect.Bottom > midY then Some ({rect with Right = min midX rect.Right; Top = max (midY + 1) rect.Top}) else None
    let bottomRightRect = if rect.Right > midX && rect.Bottom > midY then Some ({rect with Left = max (midX + 1) rect.Left; Top = max (midY + 1) rect.Top}) else None
    (topLeftRect, topRightRect, bottomLeftRect, bottomRightRect)

let rec processArea (rect : Rect) (areaRect : Rect) (area : Area option) (fn : Rect -> Rect -> (Area option) -> (unit -> Area option) -> (Area option)): Area option =
    if rect.Left > rect.Right || rect.Top > rect.Bottom || rect.Left > areaRect.Right || rect.Top > areaRect.Bottom || rect.Right < areaRect.Left || rect.Bottom < areaRect.Top then
        area
    else
        let continuation () =
            let (topLeftRect, topRightRect, bottomLeftRect, bottomRightRect) = SplitToIntersectionRects rect areaRect.MiddlePoint
            let (areaTopLeftRect, areaTopRightRect, areaBottomLeftRect, areaBottomRightRect) = areaRect.Split
            let childTopLeftArea = (Option.bind (fun (a : Area) -> a.TopLeftArea |> Option.orElseWith (fun _ -> if a.HasNoChildren then Some(Area.CreateTurned()) else None)) area)
            let childTopRightArea = (Option.bind (fun (a : Area) -> a.TopRightArea |> Option.orElseWith (fun _ -> if a.HasNoChildren then Some(Area.CreateTurned()) else None)) area)
            let childBottomLeftArea = (Option.bind (fun (a : Area) -> a.BottomLeftArea |> Option.orElseWith (fun _ -> if a.HasNoChildren then Some(Area.CreateTurned()) else None)) area)
            let childBottomRightArea = (Option.bind (fun (a : Area) -> a.BottomRightArea |> Option.orElseWith (fun _ -> if a.HasNoChildren then Some(Area.CreateTurned()) else None)) area)
            let result = {
                TopLeftArea =  ifThenElse topLeftRect (fun r -> processArea r areaTopLeftRect childTopLeftArea fn) childTopLeftArea
                TopRightArea = ifThenElse topRightRect (fun r -> processArea r areaTopRightRect childTopRightArea fn) childTopRightArea
                BottomLeftArea = ifThenElse bottomLeftRect (fun r -> processArea r areaBottomLeftRect childBottomLeftArea fn) childBottomLeftArea 
                BottomRightArea = ifThenElse bottomRightRect (fun r -> processArea r areaBottomRightRect childBottomRightArea fn) childBottomRightArea
            }
            if result.HasNoChildren then None else Some(result)
        fn rect areaRect area continuation

let turnOn (rect : Rect) (areaRect : Rect) (area : Area option) : Area option =
    processArea rect areaRect area (fun rect areaRect _ continuation ->
        if rect = areaRect then
            Some(Area.CreateTurned())
        else
            continuation()
        )
    
let turnOff (rect : Rect) (areaRect : Rect) (area : Area option) : Area option =
    processArea rect areaRect area (fun rect areaRect area continuation -> if rect = areaRect || area.IsNone then None else continuation())

let toggle (rect : Rect) (areaRect : Rect) (area : Area option) : Area option =
    processArea rect areaRect area (fun rect areaRect area continuation ->
        if rect <> areaRect then
            continuation()
        else
            match area with
            | Some a -> if a.HasNoChildren then None else continuation()
            | None -> Some(Area.CreateTurned())
        )

let mapRect = {Left = 0; Top = 0; Right = 999; Bottom = 999;}

let parseRect (line : string) : Rect =
    let array = line.Split([|','; ' '|]) |> Array.map (fun s -> if s = "through" then 0 else int s)
    {Left = array.[0]; Top = array.[1]; Right = array.[3]; Bottom = array.[4]}

let parseLine2 (line : string) (index : int) (area : Area option) fn : Area option =
    let rect = parseRect (line.Substring index)
    fn rect mapRect area

let parseLine (line : string) (area : Area option) : Area option =
    match line with
    | l when l.StartsWith("turn off") -> parseLine2 line 9 area turnOff
    | l when l.StartsWith("turn on") -> parseLine2 line 8 area turnOn
    | l when l.StartsWith("toggle") -> parseLine2 line 7 area toggle
    | _ -> area
    
let calcTurned (area : Area option) : int =
    let rec calcTurnedInner (areaRect : Rect) (area : Area option) : int =
        match area with
        | None -> 0
        | Some a -> calcTurnedForArea areaRect a
    and calcTurnedForArea (areaRect : Rect) (area : Area) : int =
        if area.HasNoChildren then
            (areaRect.Right - areaRect.Left + 1) * (areaRect.Bottom - areaRect.Top + 1)
        else
           let (areaTopLeftRect, areaTopRightRect, areaBottomLeftRect, areaBottomRightRect) = areaRect.Split
           let turnedOnTopLeft = calcTurnedInner areaTopLeftRect area.TopLeftArea
           let turnedOnTopRight = calcTurnedInner areaTopRightRect area.TopRightArea
           let turnedOnBottomLeft = calcTurnedInner areaBottomLeftRect area.BottomLeftArea
           let turnedOnBottomRight = calcTurnedInner areaBottomRightRect area.BottomRightArea
           turnedOnTopLeft + turnedOnBottomRight + turnedOnTopRight + turnedOnBottomLeft
    calcTurnedInner mapRect area    
let ``do`` =
    Checker.check (1000*1000) (calcTurned (parseLine "turn on 0,0 through 999,999" None))
    Checker.check (1000) (calcTurned (parseLine "toggle 0,0 through 999,0" None))
    Checker.check (0) (calcTurned (parseLine "turn off 499,499 through 500,500" None))
    Checker.check (1000*1000 - 4) (calcTurned (parseLine "toggle 0,0 through 999,999" (parseLine "turn on 499,499 through 500,500" None)))
    Checker.check (1000*1000 - 1) (calcTurned (parseLine "toggle 0,0 through 999,999" (parseLine "turn on 999,999 through 999,999" None)))
    Checker.check 0 (calcTurned (parseLine "toggle 499,499 through 500,500" (parseLine "turn on 499,499 through 500,500" None)))
    Checker.check 2 (calcTurned (parseLine "turn on 0,0 through 0,1" (parseLine "turn on 0,0 through 0,0" None)))
    Checker.check 2 (calcTurned (parseLine "turn on 0,0 through 0,0" (parseLine "turn on 0,0 through 0,1" None)))
    Checker.check 11 (calcTurned (parseLine "turn on 0,0 through 0,10" (parseLine "turn on 0,0 through 0,2" None)))
    Checker.check 11 (calcTurned (parseLine "turn on 0,0 through 0,2" (parseLine "turn on 0,0 through 0,10" None)))
    Checker.check 2 (calcTurned (parseLine "turn on 0,0 through 0,0" (parseLine "turn on 999,999 through 999,999" None)))
    Checker.check 15 (calcTurned (parseLine "turn off 3,3 through 3,3" (parseLine "turn on 0,0 through 3,3" None)))
    Checker.check 0 (calcTurned (parseLine "toggle 0,0 through 999,999" (parseLine "turn on 999,999 through 999,999" (parseLine "toggle 0,0 through 999,999" (parseLine "turn on 999,999 through 999,999" None)))))
    Checker.check (2) (calcTurned (parseLine "turn on 0,0 through 0,0" (parseLine "turn on 999,999 through 999,999" None)))
    Checker.check (2) (calcTurned (parseLine "toggle 0,0 through 0,0" (parseLine "toggle 999,999 through 999,999" None)))
        
    let result = System.IO.File.ReadAllLines "..\..\..\Inputs\Day6.txt" |> Array.fold (fun acc l -> parseLine l acc) None |> calcTurned |> string
    System.IO.File.WriteAllText ("..\..\..\Outputs\Day6_1.txt", result)
    0