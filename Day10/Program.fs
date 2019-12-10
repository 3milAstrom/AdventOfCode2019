// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Numerics

let rec gcd (a: float32) (b: float32) =
    let aa,bb = abs a, abs b
    if aa = (0 |> float32) then bb
    elif bb = (0 |> float32) then aa
    elif aa > bb then gcd (aa % bb) bb
    else gcd aa (bb % aa)

let normalize (point: Vector2) : Vector2=
    let g = gcd point.X point.Y
    Vector2 ((point.X / g),(point.Y / g))

let getAsteroids data=
    List.ofSeq(File.ReadLines(data))
    |> List.mapi(fun i x ->
        x.ToCharArray()
        |> List.ofSeq
        |> List.mapi(fun j y ->
            (j |> float32,i |> float32),(y |> string)
        )
        |> List.filter(fun (k,v) -> v = "#")
        |> List.map(fun (k,v) -> Vector2 ((k |> fst),(k |> snd)))
    ) |> List.collect(id)

[<EntryPoint>]
let main argv =
    let asteroids = getAsteroids "data.txt"
    let r =
        asteroids
        |> List.map(fun a ->
            let mutable tmp = []
            asteroids |> List.iter(fun b ->
                if a<>b
                then
                    let l = (b-a) |> Vector2.Normalize
                    tmp <- (tmp @ [l |> normalize])
            )
            a,tmp.Length
        )


    printfn "%A" r
    printfn "asdasd"

    0 // return an integer exit code
