// Learn more about F# at http://fsharp.org

open System
open System.IO

type Row = List<string>

type Layer = List<Row>

type Pixel = Option<string>
type Image = Map<int*int,Pixel>

let filereader (path : string) =
    List.ofSeq(File.ReadLines(path)) |> List.head

let updateList (index : int) (value : 'a) (array: List<'a>) =
    array |> List.mapi(fun i v -> if i=index then value else v)

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

let newLeast leastZeroes hd zeroes=
    match leastZeroes with
    | Some (l,_) -> if zeroes < l then Some (zeroes,hd) else leastZeroes
    | None -> Some (zeroes,hd)

let rec findLeastZeroes (layers: List<Layer>) (leastZeroes: Option<int*Layer>) =
    match layers with
    | [hd] ->
        hd |> List.fold(fun state elem -> state + (elem |> List.filter(fun x -> x = "0") |> List.length) ) 0
           |> newLeast leastZeroes hd
    | hd::tail ->
        let zeroes = hd |> List.fold(fun state elem -> state + (elem |> List.filter(fun x -> x = "0") |> List.length) ) 0
        findLeastZeroes tail (newLeast leastZeroes hd zeroes)
    |_-> failwith "asd"

let uppdateraImage (image: Layer) (lager: Layer) =
    let mutable newImage = image
    lager |> List.iteri(fun i x ->
        x |> List.iteri(fun j y ->
            let value = newImage.[i].[j]
            if value = "2" && y <> "2"
            then newImage <- updateList i (updateList j y newImage.[i]) newImage
        )
    )
    newImage

let rec createImage (image: Layer) (remainingLayers: List<Layer>) =
    match remainingLayers with
    | [hd] ->
        uppdateraImage image hd
    | hd::tail ->
        let newImage = uppdateraImage image hd
        createImage newImage tail
    | _-> failwith ""

[<EntryPoint>]
let main argv =
    let dataTransmition = filereader "data.txt"
    let width = 25
    let height = 6

    let d: List<Layer> = createLayers (dataTransmition |> dataToStringList) width height []
    let d2 = findLeastZeroes d None
    let numberofones = d2.Value |> snd |> List.fold(fun state elem -> state + (elem |> List.filter(fun x -> x = "1") |> List.length)) 0
    let numberoftwoes = d2.Value |> snd |> List.fold(fun state elem -> state + (elem |> List.filter(fun x -> x = "2") |> List.length)) 0

    printfn "Part1: %i" (numberofones * numberoftwoes)

    let topLayer = d.Head
    let image = createImage topLayer d.Tail

    image |> List.iter(fun x ->
        x |> List.iter(fun y -> if y = "1" then printf "#" else printf " ")
        printfn ""
    )

    printfn "Hello World from F#!"
    0 // return an integer exit code


//2028 high