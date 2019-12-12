namespace Util

module MathUtil =
    let rec gcd (a: int64) (b: int64) =
        if b = 0L
        then a
        else gcd b (a % b)

    let lcm (l : List<int64>) =
        let mutable ans = l.Head
        l.Tail |> List.iteri(fun i x -> ans <- (x * ans) / (gcd x ans))