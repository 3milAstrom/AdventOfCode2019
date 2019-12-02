open System.IO
open System.Runtime

let filereader (path : string) =
    List.ofSeq(File.ReadLines(path)) |> List.map(float)

let rec fuelDivider (currentFuel: float) (state : int) : int =
    let remaningFuel = int (System.Math.Floor(currentFuel / float 3)) - 2
    if remaningFuel <= 0 then state
    else (fuelDivider (float remaningFuel) (remaningFuel + state))

[<EntryPoint>]
let main argv =
    filereader "data.txt" |> List.fold(fun state item ->
        (fuelDivider item 0) + state
    ) 0
    |>  printf "%A"

    0 // return an integer exit code

    //4985158
