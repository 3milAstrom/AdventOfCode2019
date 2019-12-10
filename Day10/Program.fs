// Learn more about F# at http://fsharp.org

open System
open System.IO

let getAsteroids data=
    List.ofSeq(File.ReadLines(data))
    |> List.mapi(fun i x ->
        x.ToCharArray()
        |> List.ofSeq
        |> List.mapi(fun j y ->
            (j,i),(y |> string)
        )
        |> List.filter(fun (k,v) -> v = "#") |> List.map(fun (k,_) -> k)
    ) |> List.collect(id)


let getAngle ((x1,y1): (int*int)) ((x2,y2): (int*int)) =
    let deltaX = (x2 - x1) |> float
    let deltaY = (y2 - y1) |> float
    Math.Atan2(deltaY, deltaX);

let updateMap (map: Map<float,List<int*int>>) (point: (int*int)) (angle: float)=
    match map.TryFind angle with
    | Some v -> map.Add(angle, v @ [point])
    | None -> map.Add(angle, [point])


[<EntryPoint>]
let main argv =
    let asteroids = getAsteroids "data.txt"
    let asd =
        asteroids |> List.map(fun a ->
            let map =
                asteroids |> List.fold(fun state b ->
                    if a<>b
                    then getAngle a b |> updateMap state b
                    else state
                ) Map.empty
            a,map
        )
    let a =
        asd |> List.map(fun (pos,map) ->
            pos,map |> Map.toList |> List.map(fun (_,v) -> v)
        ) |> List.maxBy(fun (pos,c) -> c.Length)

    printfn "asdasd"

    0 // return an integer exit code
