// Learn more about F# at http://fsharp.org

open System
open System.IO

type Row = List<string>

type Layer = List<Row>

type Pixel = Option<string>
type Image = Map<int*int,Pixel>

let filereader (path : string) =
    List.ofSeq(File.ReadLines(path)) |> List.head

let dataTransmition = filereader "data.txt"

let testDataTransmition = "123456712012"

let testDataTransmition2 = "0222112222120000"

let dataToStringList (data: string) =
    [0..(data.Length - 1)] |> List.map(fun x -> data.[x] |> string)

let rec createLayer (data: List<string>) (width: int) (rows: List<Row>) : Layer=
    match data with
    | [] -> rows
    | x when x.Length >= width ->
        let row = [data.[0..width-1]]
        createLayer data.[width..(data.Length-1)] width (rows @ row)
    | _-> failwith "createLayer: Fel antal tecken"

let rec createLayers(data: List<string>) (width: int) (height: int) (layers: List<Layer>) =
    let pickSize = width * height
    match data with
    | [] -> layers
    | x when x.Length >= pickSize ->
        let layer = createLayer data.[0..(pickSize - 1)] width []
        createLayers data.[(pickSize)..(data.Length - 1)] width height (layers @ [layer])
    | x -> failwith (sprintf "createLayers: Fel antal tecken kvar %i" x.Length)

let rec findLeastZeroes (layers: List<Layer>) (leastZeroes: Option<int*Layer>) =
    match layers with
    | [hd] ->
        let zeroes = hd |> List.fold(fun state elem -> state + (elem |> List.filter(fun x -> x = "0") |> List.length) ) 0
        match leastZeroes with
        | Some (l,_) ->
            if zeroes < l
            then Some (zeroes,hd)
            else leastZeroes
        | None -> Some (zeroes,hd)
    | hd::tail ->
        let zeroes = hd |> List.fold(fun state elem -> state + (elem |> List.filter(fun x -> x = "0") |> List.length) ) 0
        let newLeast =
            match leastZeroes with
            | Some (l,_) ->
                if zeroes < l
                then Some (zeroes,hd)
                else leastZeroes
            | None -> Some (zeroes,hd)
        findLeastZeroes tail newLeast
    |_-> failwith "asd"

let setImagePixel x y pixel (image: Image) =
    match image.TryFind (x,y) with
    | Some value ->
        if value.IsNone
        then image.Add((x,y), Some pixel)
        else image
    | None -> image.Add((x,y), Some pixel)

let rec checkRow (rows: List<Row>) (currentRow: int) (image: Image) =
    match rows with
    | [hd] ->
        let mutable im = image
        hd |> List.iteri(fun i x -> if x <> "2" then im <- setImagePixel i currentRow x image )
        im
    | hd::tail ->
        let mutable im = image
        hd |> List.iteri(fun i x -> if x <> "2" then im <- setImagePixel i currentRow x image )
        checkRow tail (currentRow + 1) im
    |_-> failwith "checkRow"


let rec createImage (layers: List<Layer>) (image: Image) =
    match layers with
    | [hd] -> checkRow hd 0 image
    | hd::tail ->
        let im = checkRow hd 0 image
        createImage tail im
    |_-> failwith "createImage"

[<EntryPoint>]
let main argv =
    let width = 25
    let height = 6

    let testWidth = 3
    let testHeight = 2

    let d: List<Layer> = createLayers (dataTransmition |> dataToStringList) width height []
    let d2 = findLeastZeroes d None
    let numberofones = d2.Value |> snd |> List.fold(fun state elem -> state + (elem |> List.filter(fun x -> x = "1") |> List.length)) 0
    let numberoftwoes = d2.Value |> snd |> List.fold(fun state elem -> state + (elem |> List.filter(fun x -> x = "2") |> List.length)) 0

    printfn "Part1: %i" (numberofones * numberoftwoes)

    let w = [0..(width - 1)]
    let h = [0..(height - 1)]

    let m =
        h |> List.fold(fun (state: Map<(int*int),Option<string>>) elem ->
            let l = w |> List.map(fun x -> (x,elem))
            let asd =
                l |> List.fold(fun (s: Map<(int*int),Option<string>>) e ->
                    s.Add(e,None)
                ) state
            asd
        ) Map.empty

    let part2 =
        createImage d m
        |> Map.toList
        |> List.map(fun (_,v) -> v)
        |> List.splitInto(6)
        |> List.iter(fun x ->
            x |> List.iter(fun y ->
                match y with
                | Some v -> if v = "0" then printf "O" else printf " "
                | None -> printf " "
            )
            printfn ""
        )

    printfn "Hello World from F#!"
    0 // return an integer exit code


//2028 high