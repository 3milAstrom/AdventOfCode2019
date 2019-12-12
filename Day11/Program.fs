// Learn more about F# at http://fsharp.org

open System
open Day11
open Types

let data = "3,8,1005,8,328,1106,0,11,0,0,0,104,1,104,0,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,29,1,104,7,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,1001,8,0,55,1,2,7,10,1006,0,23,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,84,1006,0,40,1,1103,14,10,1,1006,16,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,1002,8,1,116,1006,0,53,1,1104,16,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,102,1,8,146,2,1104,9,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,172,1006,0,65,1,1005,8,10,1,1002,16,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,204,2,1104,9,10,1006,0,30,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,102,1,8,233,2,1109,6,10,1006,0,17,1,2,6,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,266,1,106,7,10,2,109,2,10,2,9,8,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,301,1,109,9,10,1006,0,14,101,1,9,9,1007,9,1083,10,1005,10,15,99,109,650,104,0,104,1,21102,1,837548789788,1,21101,0,345,0,1106,0,449,21101,0,846801511180,1,21101,0,356,0,1106,0,449,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,235244981271,0,1,21101,403,0,0,1105,1,449,21102,1,206182744295,1,21101,0,414,0,1105,1,449,3,10,104,0,104,0,3,10,104,0,104,0,21102,837896937832,1,1,21101,0,437,0,1106,0,449,21101,867965862668,0,1,21102,448,1,0,1106,0,449,99,109,2,22102,1,-1,1,21101,40,0,2,21102,1,480,3,21101,0,470,0,1106,0,513,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,475,476,491,4,0,1001,475,1,475,108,4,475,10,1006,10,507,1101,0,0,475,109,-2,2106,0,0,0,109,4,1201,-1,0,512,1207,-3,0,10,1006,10,530,21102,1,0,-3,22102,1,-3,1,21201,-2,0,2,21102,1,1,3,21102,549,1,0,1106,0,554,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,577,2207,-4,-2,10,1006,10,577,21202,-4,1,-4,1106,0,645,21202,-4,1,1,21201,-3,-1,2,21202,-2,2,3,21101,596,0,0,1106,0,554,21201,1,0,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,615,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,637,22102,1,-1,1,21101,637,0,0,105,1,512,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0"

type Tile = {painted: int; color: int}

let turnLeft direction =
    match direction with
    | Up -> Left
    | Left -> Down
    | Down -> Right
    | Right -> Up

let turnRight direction =
    match direction with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let stepOne (x,y) direction =
    match direction with
    | Up -> (x,y + 1)
    | Right -> (x + 1, y)
    | Down -> (x,y - 1)
    | Left -> (x - 1,y)

let getColor (painted: Map<int*int,Tile>) point =
    match painted.TryFind point with
    | Some v -> v.color
    | None -> 0

let paintAndReturnNextPosition (painted: Map<int*int,Tile>) ((x,y):int*int) (color: int) (direction: int) (droneDirection: Direction)=
    let newPainted =
        match painted.TryFind (x,y) with
        | Some tile ->
            if tile.color <> color
            then painted.Add((x,y), {painted = tile.painted + 1; color = color})
            else painted
        | None -> painted.Add((x,y), {painted = 1; color = color})

    let newDirection =
        match direction with
        | 0 -> turnLeft droneDirection
        | 1 -> turnRight droneDirection
        | _-> failwith ""

    newPainted, newDirection |> stepOne (x,y), newDirection

let rec runUntillStop (painted: Map<int*int,Tile>) (position:int*int) (intCompter: Types.ComputerState) (droneDirection: Direction) =
    let currentColor = getColor painted position |> int64
    let newState = IntComputer.run intCompter [currentColor]
    let cColor = newState.output.[0] |> int
    let cDirection = newState.output.[1] |> int
    let newPainted,newPos, newDirection = paintAndReturnNextPosition painted position cColor cDirection droneDirection
    if newState.stop |> not
    then runUntillStop newPainted newPos newState newDirection
    else newPainted

[<EntryPoint>]
let main argv =
    let s1 = {state = ((data.Split ',') |> List.ofSeq |> List.map(int64)); extraState = Map.empty; output = []; relativeBase =  0L; index = 0L; stop = false}
    let startMap = Map.empty
    let startTile = {painted = 0; color = 0}

    let paintedTiles = runUntillStop (startMap.Add((0,0),startTile))(0,0) s1 Up

    printfn "Part1: %A" paintedTiles.Count //2511

    let startMap2 = Map.empty
    let startTile2 = {painted = 0; color = 1}

    let paintedTiles2 =
        runUntillStop (startMap2.Add((0,0),startTile2))(0,0) s1 Up

    let paintedTilesList =
        paintedTiles2
        |> Map.toList
        |> List.map(fun (pos,tile) ->
            pos,tile.color
        ) |> List.sortBy(fun ((x,y),_) -> Math.Atan2(x |> float,y |> float))

    let minX = paintedTilesList |> List.map(fun ((x,_),_) -> x) |> List.min
    let minY = paintedTilesList |> List.map(fun ((_,y),_) -> y) |> List.min
    let maxX = paintedTilesList |> List.map(fun ((x,_),_) -> x) |> List.max
    let maxY = paintedTilesList |> List.map(fun ((_,y),_) -> y) |> List.max

    let spanX = [minX..maxX]
    let spanY = [minY..maxY] |> List.rev

    spanY
    |> List.iter(fun y ->
        spanX |> List.iter(fun x ->
            match paintedTiles2.TryFind (x,y) with
            | Some tile -> if tile.color = 1 then printf "#" else printf " "
            | None -> printf " "
        )
        printfn ""
    )

    printfn "Hello World from F#!"
    0 // return an integer exit code
