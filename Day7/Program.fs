// Learn more about F# at http://fsharp.org

open System
open System.IO
open Day7

let data = "3,8,1001,8,10,8,105,1,0,0,21,42,67,76,89,110,191,272,353,434,99999,3,9,102,2,9,9,1001,9,2,9,1002,9,2,9,1001,9,2,9,4,9,99,3,9,1001,9,4,9,102,4,9,9,101,3,9,9,1002,9,2,9,1001,9,4,9,4,9,99,3,9,102,5,9,9,4,9,99,3,9,1001,9,3,9,1002,9,3,9,4,9,99,3,9,102,3,9,9,101,2,9,9,1002,9,3,9,101,5,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99"
let testdata = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
let testdata2 = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"

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

[<EntryPoint>]
let main argv =
    let permutation = filereader "permutations.txt"
    let permutation2 = filereader "permutations2.txt"
    let testInput2 = [0;1;2;3;4]
    let ampl = ["A";"B";"C";"D";"E"]

    let o = tryPermutations data permutation []

    // let output =
    //     ampl |> List.fold(fun ((input,lastOutput): List<int>*int) _ ->
    //         let o = IntComputer.run testdata2 [input.[0]; lastOutput]
    //         input.Tail,o
    //     ) (testInput2,0) |> snd




    0 // return an integer exit code


