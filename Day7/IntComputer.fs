namespace Day7
open System
open System.IO
module IntComputer =
    let updateList (index : int) (value : int) (array: List<int>) =
        array |> List.mapi(fun i v -> if i=index then value else v)

    let getValue (state: List<int>) (index: int) (mode) =
        match mode with
        | 0 -> state.[state.[index]]
        | 1 -> state.[index]
        | _ -> (sprintf "Faulty mode: %i" mode) |> failwith

    let rec optcodeReader (state: List<int>) (input: List<int>) (output: int) (index: int) =
        let optcode = state.[index] % 100
        let mode = state.[index] / 100
        match optcode with
        | 1 ->
            let v = (getValue state (index + 1) (mode % 10)) + (getValue state (index + 2) (mode / 10))
            let newState = updateList state.[index + 3] v state
            optcodeReader newState input output (index + 4)
        | 2 ->
            let v = (getValue state (index + 1) (mode % 10)) * (getValue state (index + 2) (mode / 10))
            let newState = updateList state.[index + 3] v state
            optcodeReader newState input output (index + 4)
        | 3 ->
            if input.IsEmpty then state,output,index, false
            else
                let readInput = input.Head
                let i = state.[index + 1]
                let newState = updateList i readInput state
                optcodeReader newState input.Tail output (index + 2)
        | 4 ->
            //printfn "opt4: %A" state.[state.[index + 1]]
            optcodeReader state input state.[state.[index + 1]] (index + 2)
        | 5 ->
            if (getValue state (index + 1) (mode % 10)) <> 0
            then (getValue state (index + 2) (mode / 10)) |> optcodeReader state input output
            else optcodeReader state input output (index + 3)
        | 6 ->
            if (getValue state (index + 1) (mode % 10)) = 0
            then (getValue state (index + 2) (mode / 10)) |> optcodeReader state input output
            else optcodeReader state input output (index + 3)
        | 7 ->
            if (getValue state (index + 1) (mode % 10)) < (getValue state (index + 2) (mode / 10))
            then optcodeReader (updateList state.[index + 3] 1 state) input output (index + 4)
            else optcodeReader (updateList state.[index + 3] 0 state) input output (index + 4)
        | 8 ->
            if (getValue state (index + 1) (mode % 10)) = (getValue state (index + 2) (mode / 10))
            then optcodeReader (updateList state.[index + 3] 1 state) input output (index + 4)
            else optcodeReader (updateList state.[index + 3] 0 state) input output (index + 4)
        | 99 -> state,output,index,true
        | _-> failwith (sprintf "Error: faluty optcode %i" optcode)

    let run (data : string) input =
        let inst1 = (data.Split ',') |> List.ofSeq |> List.map(int)
        let inst2 = [1002;4;3;4;33]
        let inst3 = [3;9;8;9;10;9;4;9;99;-1;8]
        let inst4= [3;3;1108;-1;8;3;4;3;99]
        let state, output, _,_= optcodeReader inst1 input 0 0
        output

    let runSecond (state : List<int>) index input oldOutput=
        optcodeReader state input oldOutput index

