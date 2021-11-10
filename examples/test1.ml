(* for testing *)

let expr : int =
    let x = 10 
    in match x * 2 with
        | 12 -> 3 |> fun y -> y + x + x
        | t  -> if t < 0
            then !(t <= -2) && 4 < 3/4
            else let y = x*x*x in
                map (fun z -> z / 3 * 2) (-y)
