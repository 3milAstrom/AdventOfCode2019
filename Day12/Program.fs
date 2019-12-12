// Learn more about F# at http://fsharp.org

open System

type Point3D = {x: int; y: int; z: int} with 
        member this.Add (point: Point3D) = 
            {x = this.x + point.x ; y = this.y + point.y; z = this.z + point.z}

type Moon = {name: string; position: Point3D; velocity: Point3D}
type Energy = {potential: int; kinetic: int; total: int}


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

let run10times (moons: List<Moon>) =
    [0..9] |> List.fold(fun state _ ->  calculatePositions state) moons

let runXTimes (times: int) (moons: List<Moon>) =
    let i = [0..((times / 10) - 1)]
    i |> List.fold(fun (moonState, energy) _ ->
        let s = (run10times moonState)

        s,energy
    ) (moons,{potential= 0; kinetic = 0; total = 0})



[<EntryPoint>]
let main argv =
    let startVelocity = {x = 0; y = 0; z = 0}
    // let io = {name = "io"; position = {x= -2;y = 9;z = -1}; velocity = startVelocity}
    // let europa = {name = "europa"; position = {x= 16;y = 19;z = 9}; velocity = startVelocity}
    // let ganymede = {name = "ganymede"; position = {x= 0;y = 3;z = 6}; velocity = startVelocity}
    // let callisto = {name = "callisto"; position = {x= 11;y = 0;z = 11}; velocity = startVelocity}

    //Test1
    // let io = {name = "io"; position = {x= -1;y = 0;z = 2}; velocity = startVelocity}
    // let europa = {name = "europa"; position = {x= 2;y = -10;z = -7}; velocity = startVelocity}
    // let ganymede = {name = "ganymede"; position = {x= 4;y = -8;z = 8}; velocity = startVelocity}
    // let callisto = {name = "callisto"; position = {x= 3;y = 5;z = -1}; velocity = startVelocity}

    //Test2
    let io = {name = "io"; position = {x= -8;y = -10;z = 0}; velocity = startVelocity}
    let europa = {name = "europa"; position = {x= 5;y = 5;z = 10}; velocity = startVelocity}
    let ganymede = {name = "ganymede"; position = {x= 2;y = -7;z = 3}; velocity = startVelocity}
    let callisto = {name = "callisto"; position = {x= 9;y = -8;z = -3}; velocity = startVelocity}

    let moons = [io;europa;ganymede;callisto]
    let moonsAfter10 = run10times moons
    let moonsAfter20 = run10times moonsAfter10
            

    printfn ""
    0 // return an integer exit code
