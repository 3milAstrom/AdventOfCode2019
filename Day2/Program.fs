// Learn more about F# at http://fsharp.org

open System
open System.IO

let updateList (index : int) (value : int) (array: List<int>) = 
    array |> List.mapi(fun i v -> if i=index then value else v)

let filereader (path : string) : List<int> =
    List.ofSeq(File.ReadLines(path)).[0].Split ',' 
    |> List.ofSeq
    |> List.map(int)

let rec optcodeReader (state: List<int>) (index: int) : List<int> = 
    let optcode = state.[index]
    match optcode with
    | 1 -> 
        let v = state.[state.[index + 1]] + state.[state.[index + 2]]
        let newState = updateList state.[index + 3] v state
        optcodeReader newState (index + 4)
    | 2 ->
        let v = state.[state.[index + 1]] * state.[state.[index + 2]]
        let newState = updateList state.[index + 3] v state
        optcodeReader newState (index + 4)
    | 99 ->
        state
    | _-> failwith (sprintf "Error: fel värde på optcode %i" optcode)

let rec findValue (list: List<int>) baseValue currentValue toValue =
    if currentValue < toValue then
        let newInstr = list |> updateList 1 baseValue |> updateList 2 currentValue
        if optcodeReader newInstr 0 |> List.head = 19690720 
        then Some(baseValue,currentValue)
        else findValue list baseValue (currentValue+1) toValue
    elif baseValue + 1 < toValue then
        findValue list (baseValue+1) 0 toValue
    else None

[<EntryPoint>]
let main argv =
    let instructions = filereader "data.txt"
    let inst1 = instructions |> updateList 1 12 |> updateList 2 2
    optcodeReader inst1 0 |> printfn "First resut: %A"

    let foundPair = (findValue instructions 0 0 (instructions.Length - 4))
    match foundPair with 
    | Some (noun,verb) -> 
        printfn "Pair found: %i %i" noun verb 
        printfn "Value: %i" (100 * noun + verb)
    | _-> printfn "Nu pair found"

    0 // return an integer exit code

     //53 98
    //4576384
    //5398