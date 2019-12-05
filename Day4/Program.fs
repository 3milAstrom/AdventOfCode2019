// Learn more about F# at http://fsharp.org

open System

let rec isIncreasing (list: List<int>) lastNumber =
    match list with
    | [] -> true
    | [hd] -> lastNumber <= hd
    | hd::tail -> 
        if lastNumber <= hd 
        then isIncreasing tail hd
        else false

let rec hasDouble (list: List<int>) lastNumber (state: Set<int>) =
    match list with
    | [] -> failwith "asdasd"
    | [hd] -> 
        if (lastNumber = hd)
        then (state.Add hd)
        else state
    | hd::tail -> 
        if hd = lastNumber
        then hasDouble tail hd (state.Add hd)
        else hasDouble tail hd state

let rec noTripplets (list: List<int>) lastNumber evenMoreLast (state: Set<int>) =
    match list with
    | [] -> failwith ""
    | [hd] -> 
        if hd = evenMoreLast
        then (state.Add hd)
        else state
    | hd::tail -> 
        let head = hd
        if head = evenMoreLast
        then noTripplets tail hd lastNumber (state.Add hd)
        else noTripplets tail hd lastNumber state

let numberFullfillsCritera number = 
    let numberList = (string number).ToCharArray() |> Seq.map(int) |> List.ofSeq

    let inc = isIncreasing numberList.Tail numberList.Head
    let dou = hasDouble numberList.Tail numberList.Head Set.empty
    let last = numberList.Head
    let e = numberList.Tail.Head
    let noTrip = noTripplets numberList.Tail.Tail e last Set.empty

    let d = Set.difference dou noTrip
    inc && (d.Count > 0)

[<EntryPoint>]
let main argv =
    let min = 171309
    let max = 643603

    let range = [min..max]

    let totalNumber = 
        range |> List.fold(fun (state: int) (num:int) ->
            if numberFullfillsCritera num
            then state + 1
            else state
        ) 0
    printfn "Hello World from F#!"
    0 // return an integer exit code

