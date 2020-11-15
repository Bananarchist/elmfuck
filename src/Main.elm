module Main exposing (..)

import Array exposing (..)


type alias BFMachine =
    { script : Array Char
    , position : Int
    , pointer : Int
    , buffer : Array Int
    , stdout : Array Int
    , stdin : Array Int
    , stack : Array ( Int, Int )
    , waiting : Bool
    }


createMachine : BFMachine
createMachine =
    { script = initialize 0 (\_ -> ' ')
    , position = 0
    , pointer = 0
    , buffer = initialize 300000 (\_ -> 0)
    , stdout = initialize 0 (\_ -> 0)
    , stdin = initialize 0 (\_ -> 0)
    , stack = initialize 0 (\_ -> ( 0, 0 ))
    , waiting = False
    }


{-| Debug format - eventually add runlengths for optimizing BF scripts
-}
type BFOP
    = IncrPtr
    | DecrPtr
    | IncrByte
    | DecrByte
    | Write
    | Read
    | StartBlock
    | EndBlock


decodeChar : Char -> Maybe BFOP
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


goToNextCommand : BFMachine -> BFMachine
goToNextCommand machine =
    { machine | position = machine.position + 1 }


processCurrentCommand : BFMachine -> BFMachine
processCurrentCommand machine =
    let
        currentCmd =
            get machine.position machine.script
                |> Maybe.withDefault 'x'
                |> decodeChar
    in
    case currentCmd of
        Just cmd ->
            let
                currentByte =
                    get machine.pointer machine.buffer
                        |> Maybe.withDefault 0
            in
            case cmd of
                IncrPtr ->
                    { machine | pointer = machine.pointer + 1 }

                DecrPtr ->
                    { machine | pointer = machine.pointer - 1 }

                IncrByte ->
                    { machine | buffer = set machine.pointer (currentByte + 1) machine.buffer }

                DecrByte ->
                    { machine | buffer = set machine.pointer (currentByte - 1) machine.buffer }

                Write ->
                    { machine | stdout = push currentByte machine.stdout }

                Read ->
                    if length machine.stdin > 0 then
                        let
                            readByte =
                                get 0 machine.stdin
                                    |> Maybe.withDefault 0

                            newStdin =
                                slice 1 0 machine.stdin
                        in
                        { machine
                            | stdin = newStdin
                            , buffer = set machine.pointer readByte machine.buffer
                            , waiting = False
                        }

                    else
                        { machine
                            | waiting = True
                        }

                StartBlock ->
                    let
                        endPos =
                            case closingBracketForOpeningBracket machine.script machine.position of
                                Ok position ->
                                    position

                                Err error ->
                                    machine.position

                        newStack =
                            push ( machine.position, endPos ) machine.stack
                    in
                    if currentByte == 0 then
                        { machine | position = endPos }

                    else
                        { machine | stack = newStack }

                EndBlock ->
                    let
                        remainingStack =
                            slice 0 -1 machine.stack

                        unresolved =
                            get (length remainingStack) machine.stack
                                |> Maybe.withDefault ( 0, 0 )
                    in
                    if currentByte == 0 then
                        { machine | stack = remainingStack }

                    else
                        { machine | position = Tuple.second unresolved }

        Nothing ->
            machine


step : BFMachine -> BFMachine
step machine =
    case machine.waiting of
        True ->
            if length machine.stdin > 0 then
                let
                    newMachine =
                        processCurrentCommand machine
                in
                { newMachine | position = newMachine.position + 1 }

            else
                machine

        False ->
            let
                newMachine =
                    processCurrentCommand machine
            in
            { newMachine | position = newMachine.position + 1 }


flush : BFMachine -> Array Char
flush machine =
    map Char.fromCode machine.stdout


closingBracketForOpeningBracket : Array Char -> Int -> Result String Int
closingBracketForOpeningBracket script position =
    closingBracketLocatorIterator script position 1


closingBracketLocatorIterator : Array Char -> Int -> Int -> Result String Int
closingBracketLocatorIterator script position offset =
    let
        currentPosition =
            position + offset
    in
    if currentPosition >= length script then
        Err "Exceeded script length without locating closing bracket"

    else
        case get currentPosition script of
            Just '[' ->
                let
                    closingPosition =
                        closingBracketLocatorIterator script currentPosition 1
                in
                case closingPosition of
                    Ok value ->
                        let
                            newOffset =
                                value - position + 1
                        in
                        closingBracketLocatorIterator script position newOffset

                    Err e ->
                        Err e

            Just ']' ->
                Ok currentPosition

            _ ->
                closingBracketLocatorIterator script position (offset + 1)
