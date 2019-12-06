// Learn more about F# at http://fsharp.org

open System
open System.IO

let filereader (path : string) =
    List.ofSeq(File.ReadLines(path))
    |> List.map(fun x ->
        let l = x.Split ')'
        l.[1],l.[0]
    )
let testdata =
    ["COM)B";"B)C";"C)D";"D)E";"E)F";"B)G";"G)H";"D)I";"E)J";"J)K";"K)L"]
    |> List.map(fun x ->
        let l = x.Split ')'
        l.[1],l.[0]
    )

let testdata2 =
    ["COM)B";"B)C";"C)D";"D)E";"E)F";"B)G";"G)H";"D)I";"E)J";"J)K";"K)L";"K)YOU";"I)SAN"]
    |> List.map(fun x ->
        let l = x.Split ')'
        l.[1],l.[0]
    )

let rec findParent (orbits: Map<string,string>) (elem: string) (pathToCom: List<string>) (tot: int)=
    match orbits.TryFind elem with
    | Some value when value <> "COM" ->
        let newPath = pathToCom @ [elem]
        findParent orbits value newPath (tot+1)
    | Some value when value = "COM" -> pathToCom, tot + 1
    | _ -> failwith "Error"

[<EntryPoint>]
let main argv =
    let orbits = filereader "data.txt"
    orbits
    |> List.map(fst)
    |> List.sumBy(fun p -> findParent (orbits |> Map.ofList) p [] 0 |> snd ) // 186597
    |> printfn "Part1: %i"

    let pathSan = findParent (orbits |> Map.ofList) "SAN" [] 0 |> fst
    findParent (orbits |> Map.ofList) "YOU" [] 0
    |> fst
    |> List.mapi(fun i you ->
        match pathSan |> List.tryFindIndex(fun san -> san = you) with
        | Some index -> Some (index + i - 2) //412
        | None -> None
    ) |> List.choose id |> List.head
    |> printfn "Part2: %i"


    printfn "Hello World from F#!"
    0 // return an integer exit code
