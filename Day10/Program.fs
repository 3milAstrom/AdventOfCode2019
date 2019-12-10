// Learn more about F# at http://fsharp.org

open System
open System.IO

let getAsteroids data=
    List.ofSeq(File.ReadLines(data))
    |> List.mapi(fun i x ->
        x.ToCharArray() |> List.ofSeq |> List.mapi(fun j y -> (j,i),(y |> string))
        |> List.filter(fun (k,v) -> v = "#") |> List.map(fun (k,_) -> k)
    ) |> List.collect(id)

let getAngle ((x1,y1): (int*int)) ((x2,y2): (int*int)) =
    let deltaX = (x2 - x1) |> float
    let deltaY = (y2 - y1) |> float
    let angle = (Math.Atan2(deltaY, deltaX) % (2.0 * Math.PI)) + (Math.PI / 2.0);
    if angle < 0.0
    then angle + (2.0 * Math.PI)
    else angle

let updateMap (map: Map<float,List<int*int>>) (point: (int*int)) (angle: float)=
    match map.TryFind angle with
    | Some v -> map.Add(angle, v @ [point])
    | None -> map.Add(angle, [point])

let rec findAllAngles (point1: int*int) (asteroids: list<int*int>) (state: Map<float,List<int*int>>)  =
    match asteroids with
    | [hd] -> if point1<>hd then getAngle point1 hd |> updateMap state hd else state
    | hd::tail ->
        if point1<>hd
        then getAngle point1 hd |> updateMap state hd |> findAllAngles point1 tail
        else findAllAngles point1 tail state
    |_-> failwith ""

let rec vaporise (suroundingAsteroids: List<float*List<int*int>>) (count: int)=
    match suroundingAsteroids with
    | [hd] when count = 199 -> hd
    | hd::_ when count = 199 -> hd
    | hd::tail ->
        let pos,ast = hd
        if ast.Length > 1
        then vaporise (tail @ [pos,ast.Tail]) (count + 1)
        else vaporise (tail) (count + 1)
    |_-> failwith "inget hittat på 200"

[<EntryPoint>]
let main argv =
    let asteroids = getAsteroids "data.txt"
    let asteroidsWithAngles = asteroids |> List.map(fun a -> a,(findAllAngles a asteroids Map.empty))
    let posPart1,listPart1 =
        asteroidsWithAngles |> List.map(fun (pos,map) ->
            pos,map |> Map.toList |> List.map(fun (_,v) -> v)
        ) |> List.maxBy(fun (pos,c) -> c.Length) // 269

    let stationPos, suroundingAsteroids = asteroidsWithAngles |> List.filter(fun (pos, map) -> pos = posPart1) |> List.head

    let part2 = vaporise (suroundingAsteroids |> Map.toList |> List.sortBy(id)) 0 |> snd |> List.head //612

    printfn "Del1: %A" listPart1.Length
    printfn "Del2: %A" (((part2 |> fst) * 100) + (part2 |> snd))
    0 // return an integer exit code
