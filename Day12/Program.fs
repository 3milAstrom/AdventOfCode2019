// Learn more about F# at http://fsharp.org

open System
open Util
open Types
open MathFunctions

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

let run1time (moons: List<Moon>) = calculatePositions moons

let run10times (moons: List<Moon>) =
    [0..9] |> List.fold(fun state _ ->  calculatePositions state) moons

let runXTimes (times: int) (moons: List<Moon>) =
    let m = [0..((times / 10) - 1)] |> List.fold(fun moonState _ -> (run10times moonState)) moons
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
