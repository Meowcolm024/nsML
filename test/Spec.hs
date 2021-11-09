import           Parser

main :: IO ()
main = do
    case test11 of
        Left  pe -> error $ show pe
        Right ex -> print ex
  where
    test1 = regularParse sigDef "val map : ('a -> 'b) -> 'a list -> 'b list"
    test2 = regularParse typeDef "type 'a list = nil | cons of 'a * 'a list"
    test3 = regularParse funDef "val add1 : 'a -> 'a\nlet add1 x = y"
    test4 = regularParse funTypeTerm "('a -> 'b) option -> 'b option"
    test5 = regularParse funApp "map (add one) xs"
    test6 = regularParse
        matches
        "match x with | 1 -> haha | (Pair 1 (Some t)) -> hihi | (Some i) -> omg | (None) -> qaq"
    test7 = regularParse expr "!(6/(-1)+2*3==0) && (4 < 4*7+6) || !true"
    test8 = regularParse
        expr
        "let a = 5 in if f a == 2 then 4 else match a with | true -> f g false | _ -> error haha"
    test9 = regularParse
        expr
        "fun a -> let c = 3*6 in fun b -> if a + b / c < 0 then error \"haha\" else match c with | 1 -> 3 | i -> 7"
    test10 = regularParse expr "if let x = 1 in x then x <= 3 else y < x"
    test11 = regularParse
        expr
        "f x |> fun y -> if y < 0 then y |> g else y + 1 |> h unit"
