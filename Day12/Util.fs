namespace Util
open System
module Types =
    type Point3D = {x: int; y: int; z: int} with
        member this.Add (point: Point3D) =
            {x = this.x + point.x ; y = this.y + point.y; z = this.z + point.z}
    type Moon = {name: string; position: Point3D; velocity: Point3D}
    type Energy = {potential: int; kinetic: int; total: int}
    type Sequences = {xSequence: Set<List<int*int>>; ySequence: Set<List<int*int>>; zSequence: Set<List<int*int>>; xStop: bool; yStop: bool; zStop: bool} with
        member private this.AddX (x: List<int*int>) =
            if this.xStop then this elif this.xSequence.Contains x then {this with xStop = true} else {this with xSequence = this.xSequence.Add x}
        member private this.AddY (y: List<int*int>) =
            if this.yStop then this elif this.ySequence.Contains y then {this with yStop = true} else {this with ySequence = this.ySequence.Add y}
        member private this.AddZ (z: List<int*int>) =
            if this.zStop then this elif this.zSequence.Contains z then {this with zStop = true} else {this with zSequence = this.zSequence.Add z}
        member this.Add (x: List<int*int>) (y: List<int*int>) (z: List<int*int>) =
            let ax = this.AddX x
            let yx = ax.AddY y
            yx.AddZ z

module MathFunctions =
    let rec gcd (a: int64) (b: int64) =
        if b = 0L
        then a
        else gcd b (a % b)

    let lcm (l : List<int64>) =
        let mutable ans = l.Head
        l.Tail |> List.iteri(fun i x -> ans <- (x * ans) / (gcd x ans))
        ans