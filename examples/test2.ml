type 'a Option = None | Some of 'a

type 'a 'b Either = Left of 'a | Right of 'b

val map : ('a -> 'b) -> 'a list -> 'b list
let map f xs =
    match xs with
    | (nil) -> nil
    | (cons y ys) -> cons (f y) (map f ys)
