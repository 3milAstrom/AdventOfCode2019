// Learn more about F# at http://fsharp.org

open System


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

let comparePosition (coord1: int) (coord2: int) =
    if coord1 < coord2 then 1 elif coord1 > coord2 then -1 else 0

let calculateVelosity (velocity : Point3D) (point1: Point3D) (point2: Point3D) =
    {x = velocity.x + (comparePosition point1.x point2.x);
     y = velocity.y + (comparePosition point1.y point2.y);
     z = velocity.z + (comparePosition point1.z point2.z)}

let calculatePositions (moons: List<Moon>) =
    moons |> List.map(fun moon ->
        let newMoon =
            moons |> List.fold(fun state moon2 ->
                if state.name <> moon2.name
                then {state with velocity = calculateVelosity state.velocity moon.position moon2.position}
                else state
            ) moon
        {newMoon with position = newMoon.position.Add newMoon.velocity}
    )

let calculateEnergy (moons: List<Moon>) =
    let origEnergy = {potential= 0; kinetic = 0; total = 0}
    moons |> List.fold(fun energy moon ->
        let potential = Math.Abs(moon.position.x) + Math.Abs(moon.position.y) + Math.Abs(moon.position.z)
        let kinetic = Math.Abs(moon.velocity.x) + Math.Abs(moon.velocity.y) + Math.Abs(moon.velocity.z)
        let total = energy.total + (potential * kinetic)
        {potential = potential; kinetic = kinetic; total = total}
    ) origEnergy

let run1time (moons: List<Moon>) =
    calculatePositions moons

let run10times (moons: List<Moon>) =
    [0..9] |> List.fold(fun state _ ->  calculatePositions state) moons

let runXTimes (times: int) (moons: List<Moon>) =
    let i = [0..((times / 10) - 1)]
    let m =
        i |> List.fold(fun moonState _ ->
            let s = (run10times moonState)
            s
        ) moons
    calculateEnergy m

let rec runUntilRepeting (sequences: Sequences) (moons: List<Moon>) =
    if sequences.xStop && sequences.yStop && sequences.zStop
    then sequences
    else
        let x = moons |> List.map(fun moon -> moon.position.x,moon.velocity.x)
        let y = moons |> List.map(fun moon -> moon.position.y,moon.velocity.y)
        let z = moons |> List.map(fun moon -> moon.position.z,moon.velocity.z)
        let newSeq = sequences.Add x y z
        runUntilRepeting newSeq (run1time moons)

let rec gcd (a: int64) (b: int64) =
    if b = 0L
    then a
    else gcd b (a % b)

let lcm (l : List<int64>) =
    let mutable ans = l.Head
    l.Tail |> List.iteri(fun i x -> ans <- (x * ans) / (gcd x ans))
    ans


[<EntryPoint>]
let main argv =
    let startVelocity = {x = 0; y = 0; z = 0}
    let io = {name = "io"; position = {x= -2;y = 9;z = -5}; velocity = startVelocity}
    let europa = {name = "europa"; position = {x= 16;y = 19;z = 9}; velocity = startVelocity}
    let ganymede = {name = "ganymede"; position = {x= 0;y = 3;z = 6}; velocity = startVelocity}
    let callisto = {name = "callisto"; position = {x= 11;y = 0;z = 11}; velocity = startVelocity}

    let moons = [io;europa;ganymede;callisto]
    let moonsPart1 = runXTimes 1000 moons //12053

    let startSequence = {xSequence = Set.empty; ySequence = Set.empty; zSequence = Set.empty; xStop = false; yStop = false; zStop = false}

    let moonsPart2 = runUntilRepeting startSequence moons
    let part2 = lcm [moonsPart2.xSequence.Count |> int64;moonsPart2.ySequence.Count |> int64;moonsPart2.zSequence.Count |> int64] //320380285873116


    printfn ""
    0 // return an integer exit code
