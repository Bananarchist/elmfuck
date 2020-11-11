module Main exposing (BFCommand(..), BFDirection(..), BFTarget(..), decodeChar)

import Array exposing (..)


type alias BFModel =
    { script : Array Char
    , position : Int
    , pointer : Int
    , buffer : Array Int
    }


type BFTarget
    = Pointer
    | Byte


type BFDirection
    = Start
    | End


type BFCommand
    = Incr BFTarget
    | Decr BFTarget
    | Write
    | Read
    | Jump BFDirection


decodeChar : Char -> Maybe BFCommand
decodeChar char =
    case char of
        '>' ->
            Just (Incr Pointer)

        '<' ->
            Just (Decr Pointer)

        '+' ->
            Just (Incr Byte)

        '-' ->
            Just (Decr Byte)

        '.' ->
            Just Write

        ',' ->
            Just Read

        '[' ->
            Just (Jump End)

        ']' ->
            Just (Jump Start)

        _ ->
            Nothing


parseScript : String -> Array Char
parseScript script =
    script
        |> String.toList
        |> fromList


goToNextCommand : BFModel -> BFModel
goToNextCommand model =
    { model | position = model.position + 1 }


processCurrentCommand model =
    case get model.position model.script of
        Just cmd ->
            Nothing

        Nothing ->
            Nothing
