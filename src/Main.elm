module Main exposing (..)

import Array exposing (..)


type alias BFModel =
    { script : Array Char
    , position : Int
    , pointer : Int
    , buffer : Array Int
    , stdout : Array Char
    , stdin : Array Char
    , stack : Array ( Int, Int )
    }


{-| Debug format - eventually add runlengths for optimizing BF scripts
-}
type BFCommand
    = IncrPtr
    | DecrPtr
    | IncrByte
    | DecrByte
    | Write
    | Read
    | StartBlock
    | EndBlock


decodeChar : Char -> Maybe BFCommand
decodeChar char =
    case char of
        '>' ->
            Just IncrPtr

        '<' ->
            Just DecrPtr

        '+' ->
            Just IncrByte

        '-' ->
            Just DecrByte

        '.' ->
            Just Write

        ',' ->
            Just Read

        '[' ->
            Just StartBlock

        ']' ->
            Just EndBlock

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


processCurrentCommand : BFModel -> BFModel
processCurrentCommand model =
    let
        currentCmd =
            get model.position model.script
                |> Maybe.withDefault 'x'
                |> decodeChar
    in
    case currentCmd of
        Just cmd ->
            let
                currentByte =
                    get model.pointer model.buffer
                        |> Maybe.withDefault 0
            in
            case cmd of
                IncrPtr ->
                    { model | pointer = model.pointer + 1 }

                DecrPtr ->
                    { model | pointer = model.pointer - 1 }

                IncrByte ->
                    { model | buffer = set model.pointer (currentByte + 1) model.buffer }

                DecrByte ->
                    { model | buffer = set model.pointer (currentByte - 1) model.buffer }

                Write ->
                    { model | stdout = push (Char.fromCode currentByte) model.stdout }

                Read ->
                    let
                        readByte =
                            'x'
                                |> Char.toCode

                        newStdin =
                            slice 1 0 model.stdin
                    in
                    { model
                        | stdin = newStdin
                        , buffer = set model.pointer readByte model.buffer
                    }

                _ ->
                    model

        Nothing ->
            model
