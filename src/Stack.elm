module Stack exposing (Stack, map, new, peak, pop, push, toList)


type alias Stack a =
    List a


new : Stack a
new =
    []


map =
    List.map


toList =
    identity


peak : Stack a -> Result String a
peak stack =
    case List.head stack of
        Just val ->
            Ok val

        Nothing ->
            Err "Empty stack"


pop : Stack a -> Stack a
pop stack =
    case stack of
        _ :: xs ->
            xs

        _ ->
            stack


push : a -> Stack a -> Stack a
push =
    (::)
