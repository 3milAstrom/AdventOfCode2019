namespace Day13
open System
open System.IO
open Types
module IntComputer =

    let updateList (index : int64) (relativeBase: int64) (value : int64) (array: List<int64>) (extraState: Map<int64,int64>) (mode: int64) =
        let pos =
            match mode % 10L with
            | 0L -> array.[index |> int]
            | 2L -> relativeBase + array.[index |> int]
            | _-> failwith ""

        if pos < (int64 array.Length)
        then array |> List.mapi(fun i v -> if i=(int pos) then value else v), extraState
        else array, extraState.Add(pos, value)

    let getValue (state: List<int64>) (extraState: Map<int64,int64>) (index: int64) (relativeBase: int64) (mode: int64) =
        match mode % 10L with
        | 0L ->
            let pos = state.[index |> int]
            if pos < (state.Length |> int64)
            then state.[pos |> int]
            else match extraState.TryFind (int64 pos) with | Some v -> v | None -> 0L
        | 1L -> state.[index |> int]
        | 2L ->
            let pos = relativeBase + state.[index |> int]
            if pos < (int64 state.Length)
            then state.[pos |> int]
            else match extraState.TryFind (int64 pos) with | Some v -> v | None -> 0L
        | _ -> (sprintf "Faulty mode: %i" mode) |> failwith

    let rec optcodeReader (state: List<int64>) (extraState: Map<int64,int64>) (input: List<int64>) (output: List<int64>) (relativeBase: int64) (index: int64) =
        let asd = state.[index |> int]
        let optcode = state.[index |> int] % (100L)
        let mode = state.[index |> int] / (100L)
        match optcode with
        | 1L ->
            let v = (getValue state extraState (index + 1L) relativeBase mode) + (getValue state extraState (index + 2L) relativeBase (mode / 10L))
            let newState, newExtraState = updateList (index + 3L) relativeBase v state extraState ((mode / 10L)/ 10L)
            optcodeReader newState newExtraState input output relativeBase (index + 4L)
        | 2L ->
            let v = (getValue state extraState (index + 1L) relativeBase mode ) * (getValue state extraState (index + 2L) relativeBase (mode / 10L))
            let newState, newExtraState = updateList (index + 3L) relativeBase  v state extraState ((mode / 10L)/ 10L)
            optcodeReader newState newExtraState input output relativeBase (index + 4L)
        | 3L ->
            if input.IsEmpty then state,extraState,output,relativeBase,index, false
            else
                let readInput = input.Head
                let newState, newExtraState = updateList (index + 1L) relativeBase readInput state extraState mode
                optcodeReader newState newExtraState input.Tail output relativeBase (index + 2L)
        | 4L ->
            let o = getValue state extraState (index + 1L) relativeBase mode
            printfn "%A" o
            optcodeReader state extraState input (output @ [o]) relativeBase (index + 2L)
        | 5L ->
            if (getValue state extraState (index + 1L) relativeBase mode) <> 0L
            then (getValue state extraState (index + 2L) relativeBase (mode / 10L)) |> optcodeReader state extraState input output relativeBase
            else optcodeReader state extraState input output relativeBase (index + 3L)
        | 6L ->
            if (getValue state extraState (index + 1L) relativeBase mode) = 0L
            then (getValue state extraState  (index + 2L) relativeBase (mode / 10L)) |> optcodeReader state extraState input output relativeBase
            else optcodeReader state extraState input output relativeBase (index + 3L)
        | 7L ->
            if (getValue state extraState (index + 1L) relativeBase mode) < (getValue state extraState (index + 2L) relativeBase (mode / 10L))
            then
                let newState, newExtraState = (updateList (index + 3L) relativeBase 1L state extraState ((mode / 10L)/ 10L))
                optcodeReader newState newExtraState input output relativeBase (index + 4L)
            else
                let newState, newExtraState = (updateList (index + 3L) relativeBase 0L state extraState ((mode / 10L)/ 10L))
                optcodeReader newState newExtraState input output relativeBase (index + 4L)
        | 8L ->
            if (getValue state extraState (index + 1L) relativeBase mode) = (getValue state extraState (index + 2L) relativeBase (mode / 10L))
            then
                let newState, newExtraState = (updateList (index + 3L) relativeBase 1L state extraState ((mode / 10L)/ 10L))
                optcodeReader newState newExtraState input output relativeBase (index + 4L)
            else
                let newState, newExtraState = (updateList (index + 3L) relativeBase 0L state extraState ((mode / 10L)/ 10L))
                optcodeReader newState newExtraState input output relativeBase (index + 4L)
        | 9L ->
            let newRelativeBase = relativeBase + (getValue state extraState (index + 1L) relativeBase mode)
            optcodeReader state extraState input output newRelativeBase (index + 2L)
        | 99L -> state,extraState,output,relativeBase,index,true
        | _-> failwith (sprintf "Error: faluty optcode %i" optcode)

    let run (state: ComputerState) input =
        let s,e,o,r,i,stop = optcodeReader state.state state.extraState input [] state.relativeBase state.index
        let d: Types.ComputerState = {state = s; extraState = e;output = o; relativeBase = r; index = i; stop = stop}
        d


