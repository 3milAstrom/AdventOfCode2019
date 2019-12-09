namespace Day9
open System
open System.IO
module IntComputer =

    let updateList (index : int64) (value : int64) (array: List<int64>) =
        array |> List.mapi(fun i v -> if i=(int index) then value else v)

    let getValue (state: List<int64>) (index: int64) (relativeBase: int64) (mode: int64) =
        match mode with
        | 0L -> state.[state.[index |> int] |> int]
        | 1L -> state.[index |> int]
        | 2L -> relativeBase
        | _ -> (sprintf "Faulty mode: %i" mode) |> failwith

    let rec optcodeReader (state: List<int64>) (input: List<int64>) (output: int64) (relativeBase: int64) (index: int64) =
        let optcode = state.[index |> int] % (100L)
        let mode = state.[index |> int] / (100L)
        match optcode with
        | 1L ->
            let v = (getValue state (index + 1L) (relativeBase) (mode % 10L)) + (getValue state (index + 2L) relativeBase (mode / 10L))
            let newState = updateList state.[(index |> int) + 3] v state
            optcodeReader newState input output relativeBase (index + 4L)
        | 2L ->
            let v = (getValue state (index + 1L) relativeBase (mode % 10L)) * (getValue state (index + 2L) relativeBase (mode / 10L))
            let newState = updateList state.[(index |> int) + 3] v state
            optcodeReader newState input output relativeBase (index + 4L)
        | 3L ->
            if input.IsEmpty then state,output,index, false
            else
                let readInput = input.Head
                let i = state.[(index |> int) + 1]
                let newState = updateList i readInput state
                optcodeReader newState input.Tail output relativeBase (index + 2L)
        | 4L ->
            //printfn "opt4: %A" state.[state.[index + 1]]
            optcodeReader state input state.[state.[(index + 1L) |> int] |> int] relativeBase (index + 2L)
        | 5L ->
            if (getValue state (index + 1L) relativeBase (mode % 10L)) <> 0L
            then (getValue state (index + 2L) relativeBase (mode / 10L)) |> optcodeReader state input output relativeBase
            else optcodeReader state input output relativeBase (index + 3L)
        | 6L ->
            if (getValue state (index + 1L) relativeBase (mode % 10L)) = 0L
            then (getValue state (index + 2L) relativeBase (mode / 10L)) |> optcodeReader state input output relativeBase
            else optcodeReader state input output relativeBase (index + 3L)
        | 7L ->
            if (getValue state (index + 1L) relativeBase (mode % 10L)) < (getValue state (index + 2L) relativeBase (mode / 10L))
            then optcodeReader (updateList state.[(index |> int) + 3] 1L state) input output relativeBase (index + 4L)
            else optcodeReader (updateList state.[(index |> int) + 3] 0L state) input output relativeBase (index + 4L)
        | 8L ->
            if (getValue state (index + 1L) relativeBase (mode % 10L)) = (getValue state (index + 2L) relativeBase (mode / 10L))
            then optcodeReader (updateList state.[(index |> int) + 3] 1L state) input output relativeBase (index + 4L)
            else optcodeReader (updateList state.[(index |> int) + 3] 0L state) input output relativeBase (index + 4L)
        | 9L ->
            let v = (getValue state (index + 1L) relativeBase (mode % 10L)) + (getValue state (index + 2L) relativeBase (mode / 10L))
            optcodeReader state input state.[state.[(index + 1L) |> int] |> int] relativeBase (index + 2L)
        | 99L -> state,output,index,true
        | _-> failwith (sprintf "Error: faluty optcode %i" optcode)

    let run (data : string) input =
        //let inst1 = (data.Split ',') |> List.ofSeq |> List.map(int64)
        
        let testInst = [109L;1L;204L;-1L;1001L;100L;1L;100L;1008L;100L;16L;101L;1006L;101L;0L;99L]
        // let testInst2 = [1102;34915192;34915192;7;4;7;99;0]
        // let testInst3: List<bigint> = [bigint 104;bigint 1125899906842624;bigint 99]


        let state, output, _,_= optcodeReader testInst input 0L 0L 0L
        output

    // let runSecond (state : List<int>) index input oldOutput=
    //     optcodeReader state input oldOutput 0 index

