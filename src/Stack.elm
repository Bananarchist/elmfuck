module Stack exposing (Stack, pop, popped, push)


type alias Stack a =
    List a


pop : Stack a -> Result String a
pop stack =
    case stack of
        x :: [] ->
            Ok x

        x :: xs ->
            pop xs

        _ ->
            Err "Empty stack"


popped : Stack a -> Stack a
popped stack =
    case stack of
        x :: [] ->
            []

        x :: xs ->
            x :: popped xs

        [] ->
            []


push : Stack a -> a -> Stack a
push stack element =
    List.append stack [ element ]
