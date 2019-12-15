// Learn more about F# at http://fsharp.org

open System
open System.IO

type OreConvertion = {ore: string; count: double}

let readFile (path: string) =
    File.ReadAllLines(path) |> List.ofArray

let createEntry (x: string) =
    let c = x.Split("=>")
    let compounds = c.[0]
    let solution = c.[1].Trim().Split(' ')
    let solutionOre = {ore = solution.[1]; count = solution.[0] |> double}

    let compoundList =
        compounds.Split(',')
        |> Seq.map(fun y ->
            let s = y.Trim().Split(' ')
            {ore = s.[1]; count = s.[0] |> double}
        )
    solutionOre, compoundList |> List.ofSeq

let createMap (oreList: List<string>) =
    oreList |> List.map(createEntry) |> Map.ofList

let rec calculateCost (map: Map<OreConvertion, List<OreConvertion>>) (ore: string) =
    let currentOre = map |> Map.tryFindKey(fun k v -> k.ore = ore) |> Option.get
    let currentList = map.TryFind currentOre |> Option.get
    let cost =
        currentList |> List.fold(fun state x ->
            if x.ore <> "ORE"
            then state + Math.Ceiling((calculateCost map x.ore) / currentOre.count)
            else state + x.count
        ) 0.0

    cost






[<EntryPoint>]
let main argv =
    let map = readFile "data.txt" |> createMap
    let start,_ = map |> Map.toList |> List.filter(fun (k,_) -> k.ore = "FUEL") |> List.head
    let c = calculateCost map start.ore

    printfn "Hello World from F#!"
    0 // return an integer exit code
