module Stack exposing (Stack, pop, popped, push)

import Array as Array


type alias Stack a =
    Array.Array a


pop : Stack a -> Result String a
pop stack =
    case Array.get -1 stack of
        Just val ->
            Ok val

        Nothing ->
            Err "Empty stack"


popped : Stack a -> Stack a
popped stack =
    if Array.isEmpty stack then
        stack

    else
        Array.slice 0 -1 stack


push : a -> Stack a -> Stack a
push =
    Array.push
