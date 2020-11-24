module Brainfuck exposing (..)

import Array exposing (..)
import Queue as Q
import Stack as Stack


type Flag
    = Waiting
    | Error String
    | Done


type alias BFMachine =
    { script : Array Char
    , position : Int
    , pointer : Int
    , buffer : Array Int
    , stdout : Array Int
    , stdin : Q.Queue Int
    , stack : Stack.Stack ( Int, Int )
    , waiting : Bool
    , flag : Maybe Flag
    }


createMachine : BFMachine
createMachine =
    { script = initialize 0 (\_ -> ' ')
    , position = 0
    , pointer = 0
    , buffer = initialize 300000 (\_ -> 0)
    , stdout = fromList []
    , stdin = []
    , stack = fromList []
    , waiting = False
    , flag = Nothing
    }


initMachine : String -> BFMachine
initMachine script =
    { createMachine
        | script = parseScript script
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
                    if Q.enqueued machine.stdin then
                        let
                            readByte =
                                Q.dequeue machine.stdin
                                    |> Maybe.withDefault 0

                            newStdin =
                                Q.dequeued machine.stdin
                        in
                        { machine
                            | stdin = newStdin
                            , buffer = set machine.pointer readByte machine.buffer
                            , flag = Nothing
                        }

                    else
                        { machine
                            | flag = Just Waiting
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
                            Stack.push ( machine.position, endPos ) machine.stack
                    in
                    if currentByte == 0 then
                        { machine | position = endPos }

                    else
                        { machine | stack = newStack }

                EndBlock ->
                    let
                        remainingStack =
                            Stack.popped machine.stack

                        unresolved =
                            Stack.pop machine.stack
                                |> Result.withDefault ( 0, 0 )
                    in
                    if currentByte == 0 then
                        { machine | stack = remainingStack }

                    else
                        { machine | position = Tuple.second unresolved }

        Nothing ->
            machine


step : BFMachine -> BFMachine
step machine =
    case machine.flag of
        Just Waiting ->
            if Q.enqueued machine.stdin then
                let
                    newMachine =
                        processCurrentCommand machine
                in
                { newMachine | position = newMachine.position + 1 }

            else
                machine

        Nothing ->
            let
                newMachine =
                    processCurrentCommand machine
            in
            { newMachine | position = newMachine.position + 1 }

        _ ->
            machine


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
