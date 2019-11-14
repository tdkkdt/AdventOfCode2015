module AOC2015.Day6_2

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
    Value : int64
    }
with
    static member Create (value : int64) : Area = {TopLeftArea = None; TopRightArea = None; BottomLeftArea = None; BottomRightArea = None; Value = value}
    
let ifThenElse (ifFn : 'T -> 'U option) (elseOpt : 'U option) (option : 'T option) : 'U option =
    match option with
    | Some a -> ifFn a
    | None -> elseOpt    

let SplitToIntersectionRects (rect : Rect) (midX : int, midY : int) : (Rect option * Rect option * Rect option * Rect option) =
    let topLeftRect = if rect.Left <= midX && rect.Top <= midY then Some ({rect with Bottom = min midY rect.Bottom; Right = min midX rect.Right}) else None
    let topRightRect = if rect.Right > midX && rect.Top <= midY then Some ({rect with Left = max (midX + 1) rect.Left; Bottom = min midY rect.Bottom}) else None
    let bottomLeftRect = if rect.Left <= midX && rect.Bottom > midY then Some ({rect with Right = min midX rect.Right; Top = max (midY + 1) rect.Top}) else None
    let bottomRightRect = if rect.Right > midX && rect.Bottom > midY then Some ({rect with Left = max (midX + 1) rect.Left; Top = max (midY + 1) rect.Top}) else None
    (topLeftRect, topRightRect, bottomLeftRect, bottomRightRect)

let rec processArea (rect : Rect) (areaRect : Rect) (inc : int64) (area : Area): Area =
    if rect.Left > rect.Right || rect.Top > rect.Bottom || rect.Left > areaRect.Right || rect.Top > areaRect.Bottom || rect.Right < areaRect.Left || rect.Bottom < areaRect.Top then
        area
    else
        if rect = areaRect && area.TopLeftArea.IsNone then
            {area with Value = max 0L (area.Value + inc)}
        else 
            let (topLeftRect, topRightRect, bottomLeftRect, bottomRightRect) = SplitToIntersectionRects rect areaRect.MiddlePoint
            let (areaTopLeftRect, areaTopRightRect, areaBottomLeftRect, areaBottomRightRect) = areaRect.Split
            let getChildArea (currentChildArea : Area option) : Area = 
                currentChildArea |> Option.map (fun c -> {c with Value = c.Value + area.Value}) |> Option.defaultWith (fun _ -> Area.Create(area.Value))
            let childTopLeftArea = getChildArea area.TopLeftArea
            let childTopRightArea = getChildArea area.TopRightArea
            let childBottomLeftArea = getChildArea area.BottomLeftArea
            let childBottomRightArea = getChildArea area.BottomRightArea            
            let result = {
                TopLeftArea = topLeftRect |> Option.map (fun childRect -> processArea childRect areaTopLeftRect inc childTopLeftArea) |> Option.orElse (childTopLeftArea |> Some)
                TopRightArea = topRightRect |> Option.map (fun childRect -> processArea childRect areaTopRightRect inc childTopRightArea) |> Option.orElse (childTopRightArea |> Some)
                BottomLeftArea = bottomLeftRect |> Option.map (fun childRect -> processArea childRect areaBottomLeftRect inc childBottomLeftArea) |> Option.orElse (childBottomLeftArea |> Some) 
                BottomRightArea = bottomRightRect |> Option.map (fun childRect -> processArea childRect areaBottomRightRect inc childBottomRightArea) |> Option.orElse (childBottomRightArea |> Some)
                Value = 0L
            }
            result

let turnOn (rect : Rect) (areaRect : Rect) (area : Area) : Area = processArea rect areaRect 1L area
    
let turnOff (rect : Rect) (areaRect : Rect) (area : Area) : Area = processArea rect areaRect -1L area

let toggle (rect : Rect) (areaRect : Rect) (area : Area) : Area = processArea rect areaRect 2L area

let mapRect = {Left = 0; Top = 0; Right = 999; Bottom = 999;}

let parseRect (line : string) : Rect =
    let array = line.Split([|','; ' '|]) |> Array.map (fun s -> if s = "through" then 0 else int s)
    {Left = array.[0]; Top = array.[1]; Right = array.[3]; Bottom = array.[4]}

let parseLine2 (line : string) (index : int) (area : Area) fn : Area =
    let rect = parseRect (line.Substring index)
    fn rect mapRect area

let parseLine (line : string) (area : Area) : Area =
    match line with
    | l when l.StartsWith("turn off") -> parseLine2 line 9 area turnOff
    | l when l.StartsWith("turn on") -> parseLine2 line 8 area turnOn
    | l when l.StartsWith("toggle") -> parseLine2 line 7 area toggle
    | _ -> area
    
let calcTurned (area : Area) : int64 =
    let rec calcTurnedInner (areaRect : Rect) (area : Area) : int64 =
        let (areaTopLeftRect, areaTopRightRect, areaBottomLeftRect, areaBottomRightRect) = areaRect.Split
        let turnedOnTopLeft = area.TopLeftArea |> Option.map (calcTurnedInner areaTopLeftRect) |> Option.defaultValue 0L
        let turnedOnTopRight = area.TopRightArea |> Option.map (calcTurnedInner areaTopRightRect) |> Option.defaultValue 0L
        let turnedOnBottomLeft = area.BottomLeftArea |> Option.map (calcTurnedInner areaBottomLeftRect) |> Option.defaultValue 0L
        let turnedOnBottomRight = area.BottomRightArea |> Option.map (calcTurnedInner areaBottomRightRect) |> Option.defaultValue 0L
        let areaSize = (areaRect.Right - areaRect.Left + 1) * (areaRect.Bottom - areaRect.Top + 1)
        (int64(areaSize) * area.Value) + turnedOnTopLeft + turnedOnBottomRight + turnedOnTopRight + turnedOnBottomLeft
    calcTurnedInner mapRect area
    
let ``do`` =
    Checker.check (1000L*1000L) (calcTurned (parseLine "turn on 0,0 through 999,999" (Area.Create(0L))))
    Checker.check (2000L) (calcTurned (parseLine "toggle 0,0 through 999,0" (Area.Create(0L))))
    Checker.check (0L) (calcTurned (parseLine "turn off 499,499 through 500,500" (Area.Create(0L))))
    Checker.check (1000L*1000L * 2L + 4L) (calcTurned (parseLine "toggle 0,0 through 999,999" (parseLine "turn on 499,499 through 500,500" (Area.Create(0L)))))
    Checker.check (1000L*1000L * 2L + 1L) (calcTurned (parseLine "toggle 0,0 through 999,999" (parseLine "turn on 999,999 through 999,999" (Area.Create(0L)))))
    Checker.check 3L (calcTurned (parseLine "turn on 0,0 through 0,1" (parseLine "turn on 0,0 through 0,0" (Area.Create(0L)))))
    Checker.check 3L (calcTurned (parseLine "turn on 0,0 through 0,0" (parseLine "turn on 0,0 through 0,1" (Area.Create(0L)))))
    Checker.check 14L (calcTurned (parseLine "turn on 0,0 through 0,10" (parseLine "turn on 0,0 through 0,2" (Area.Create(0L)))))
    Checker.check 14L (calcTurned (parseLine "turn on 0,0 through 0,2" (parseLine "turn on 0,0 through 0,10" (Area.Create(0L)))))
    Checker.check 2L (calcTurned (parseLine "turn on 0,0 through 0,0" (parseLine "turn on 999,999 through 999,999" (Area.Create(0L)))))
    Checker.check 15L (calcTurned (parseLine "turn off 3,3 through 3,3" (parseLine "turn on 0,0 through 3,3" (Area.Create(0L)))))
    Checker.check (4000002L) (calcTurned (parseLine "toggle 0,0 through 999,999" (parseLine "turn on 999,999 through 999,999" (parseLine "toggle 0,0 through 999,999" (parseLine "turn on 999,999 through 999,999" (Area.Create(0L)))))))
    Checker.check (2L) (calcTurned (parseLine "turn on 0,0 through 0,0" (parseLine "turn on 999,999 through 999,999" (Area.Create(0L)))))
    Checker.check (4L) (calcTurned (parseLine "toggle 0,0 through 0,0" (parseLine "toggle 999,999 through 999,999" (Area.Create(0L)))))
    Checker.check (0L) (0L |> Area.Create |> parseLine "turn on 0,0 through 3,3" |> parseLine "turn off 0,0 through 999,999" |> calcTurned)
    let result = System.IO.File.ReadAllLines "..\..\..\Inputs\Day6.txt" |> Array.fold (fun acc l -> parseLine l acc) (Area.Create(0L)) |> calcTurned |> string
    System.IO.File.WriteAllText ("..\..\..\Outputs\Day6_2.txt", result)
    0