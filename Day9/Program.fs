// Learn more about F# at http://fsharp.org

open System
open Day9

[<EntryPoint>]
let main argv =
    let u = IntComputer.run "" []
    printfn "Hello World from F#!"
    0 // return an integer exit code
