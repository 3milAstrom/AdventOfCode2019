// Learn more about F# at http://fsharp.org

open System
open System.IO
open Day7

let dataInput = "3,8,1001,8,10,8,105,1,0,0,21,42,67,76,89,110,191,272,353,434,99999,3,9,102,2,9,9,1001,9,2,9,1002,9,2,9,1001,9,2,9,4,9,99,3,9,1001,9,4,9,102,4,9,9,101,3,9,9,1002,9,2,9,1001,9,4,9,4,9,99,3,9,102,5,9,9,4,9,99,3,9,1001,9,3,9,1002,9,3,9,4,9,99,3,9,102,3,9,9,101,2,9,9,1002,9,3,9,101,5,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99"
let testdata = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
let testdata2 = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"

let testdata3 = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
let testdata4 = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"

let filereader (path : string) =
    List.ofSeq(File.ReadLines(path))
    |> List.ofSeq
    |> List.map(fun x -> (x.Split ',') |> List.ofSeq |> List.map(int))

let runIntComputer data permutation =
    let ampl = ["A";"B";"C";"D";"E"]
    ampl |> List.fold(fun ((input,lastOutput): List<int>*int) _ ->
            let o = IntComputer.run data [input.[0]; lastOutput]
            input.Tail,o
        ) (permutation,0) |> snd

let rec tryPermutations (data: string) (permutation: List<List<int>>) (outputs: List<int>) =
    match permutation with
    | [hd] -> (outputs @ [runIntComputer data hd]) |> List.max
    | hd::tail ->
        let output = runIntComputer data hd
        tryPermutations data tail (outputs @ [output])
    | _-> failwith ""


let rec runEachComputer (data: string) (ampl: List<string>) (input: List<int>) (lastOutput: int) (states: Map<string,List<int>*int*int>)  =
    match ampl with
    | [hd] ->
        let state,index,oldOutput =
            match states.TryFind hd with
            | Some x -> x
            | None -> (data.Split ',') |> List.ofSeq |> List.map(int),0,0
        let nextInput =
            if input.IsEmpty
            then [lastOutput]
            else [input.[0]; lastOutput]
        let _,o,_,stop = IntComputer.runSecond state index nextInput oldOutput
        o
    | hd::tail ->
        let state,index,oldOutput =
            match states.TryFind hd with
            | Some x -> x
            | None -> (data.Split ',') |> List.ofSeq |> List.map(int),0,0
        let nextInput = if input.IsEmpty then [lastOutput] else [input.[0]; lastOutput]
        let (rstate,o,rindex,stop) = IntComputer.runSecond state index nextInput oldOutput
        let rInput = if input.IsEmpty then [] else input.Tail
        if stop
        then runEachComputer data tail rInput o (states.Remove hd)
        else
            let newStates = states.Add(hd,(rstate,rindex,o))
            runEachComputer data (tail @ [hd]) rInput o newStates
    | _-> failwith ""


let runIntComputer2 data permutation =
    let ampl = ["A";"B";"C";"D";"E"]
    runEachComputer data ampl permutation 0 Map.empty

let rec tryPermutations2 (data: string) (permutation: List<List<int>>) (outputs: List<int>) =
    match permutation with
    | [hd] -> (outputs @ [runIntComputer2 data hd]) |> List.max
    | hd::tail ->
        let output = runIntComputer2 data hd
        tryPermutations2 data tail (outputs @ [output])
    | _-> failwith ""

[<EntryPoint>]
let main argv =
    let permutation = filereader "permutations.txt"
    let permutation2 = filereader "permutations2.txt"

    tryPermutations dataInput permutation [] //24625
    |> printfn "Del1: %i"
    tryPermutations2 dataInput permutation2 [] //36497698
    |> printfn "Del2: %i"


    0 // return an integer exit code


