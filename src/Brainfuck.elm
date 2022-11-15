module Brainfuck exposing (..)

import Array exposing (..)
import BFParser as Parser
import Stack as Stack


type alias ScriptPosition =
    Int


type alias BufferPointer =
    Int


type alias CodeLabel =
    String


updateBufferAtPointer : (Int -> Int) -> BFMachine -> BFMachine
updateBufferAtPointer predicate state =
    { state
        | buffer =
            Array.set
                state.pointer
                (Array.get state.pointer state.buffer |> Maybe.map predicate |> Maybe.withDefault 0)
                state.buffer
    }


set : Int -> BFMachine -> BFMachine
set x =
    updateBufferAtPointer (always x)


input : BFMachine -> Char -> BFMachine
input machine char =
    set (Char.toCode char) machine
        |> processed Parser.Read


bufferLength : Int
bufferLength =
    500


type State
    = Running
    | Stdin
    | Stdout Char
    | Complete (Result String ())


type alias Script =
    List Parser.BFOP


type alias Processed =
    List Parser.BFOP


type alias BFMachine =
    { script : Script
    , processed : Processed
    , pointer : Int
    , buffer : Array Int
    , stdout : List Char
    , stack : Stack.Stack ( Processed, Script )
    , status : State
    }


createMachine : BFMachine
createMachine =
    { script = []
    , processed = []
    , pointer = 0
    , buffer = initialize bufferLength (\_ -> 0)
    , stdout = []
    , stack = []
    , status = Running
    }


initMachine : String -> BFMachine
initMachine script =
    case Parser.parse script of
        Ok parsedScript ->
            { createMachine | script = parsedScript }

        Err e ->
            { createMachine | status = Complete (Err e) }


runMachine : BFMachine -> BFMachine
runMachine machine =
    processCurrentCommand machine


movePointer : Int -> BFMachine -> BFMachine
movePointer to machine =
    let
        newPtr =
            machine.pointer + to
    in
    if newPtr >= bufferLength then
        { machine
            | status =
                [ "Max buffer length"
                , String.fromInt bufferLength
                , "exceeded, pointer forwarded to"
                , String.fromInt newPtr
                ]
                    |> String.join " "
                    |> Err
                    |> Complete
        }

    else
        { machine | pointer = newPtr }


printCurrentCharacter : BFMachine -> BFMachine
printCurrentCharacter machine =
    case get machine.pointer machine.buffer of
        Just x ->
            { machine
                | stdout =
                    Char.fromCode x
                        |> List.singleton
                        |> (++) machine.stdout
            }

        Nothing ->
            { machine
                | status =
                    [ "Couldn't read buffer at position "
                    , String.fromInt machine.pointer
                    ]
                        |> String.join " "
                        |> Err
                        |> Complete
            }


updateScript : List Parser.BFOP -> BFMachine -> BFMachine
updateScript ops machine =
    { machine | script = ops }


pushReturnStack : List Parser.BFOP -> BFMachine -> BFMachine
pushReturnStack returnOps machine =
    { machine
        | stack = Stack.push ( machine.processed, returnOps ) machine.stack
        , processed = []
    }


returnStack : BFMachine -> BFMachine
returnStack machine =
    Stack.peak machine.stack
        |> Result.map
            (\( proc, scr ) ->
                if get machine.pointer machine.buffer == Just 0 then
                    { machine
                        | stack = Stack.pop machine.stack
                        , script = scr
                        , processed = machine.processed |> Parser.Block |> List.singleton |> (++) proc
                    }

                else
                    { machine
                        | script = machine.processed
                        , processed = []
                    }
            )
        |> Result.withDefault { machine | status = Complete (Ok ()) }


running : BFMachine -> BFMachine
running machine =
    { machine | status = Running }


processed : Parser.BFOP -> BFMachine -> BFMachine
processed op machine =
    { machine | processed = op |> List.singleton |> (++) machine.processed }


mapIncomplete : (BFMachine -> BFMachine) -> BFMachine -> BFMachine
mapIncomplete fn machine =
    case machine.status of
        Complete _ ->
            machine

        _ ->
            fn machine


processCurrentCommand : BFMachine -> BFMachine
processCurrentCommand machine =
    case machine.script of
        [] ->
            machine
                |> returnStack
                |> mapIncomplete processCurrentCommand

        (Parser.Block block) :: cmds ->
            machine
                |> updateScript block
                |> pushReturnStack cmds
                |> running
                |> processCurrentCommand

        Parser.Print :: cmds ->
            printCurrentCharacter machine
                |> updateScript cmds
                |> processed Parser.Print
                |> running
                |> processCurrentCommand

        (Parser.Byte sum) :: cmds ->
            updateBufferAtPointer ((+) sum) machine
                |> updateScript cmds
                |> processed (Parser.Byte sum)
                |> running
                |> processCurrentCommand

        (Parser.Ptr to) :: cmds ->
            movePointer to machine
                |> updateScript cmds
                |> processed (Parser.Ptr to)
                |> running
                |> processCurrentCommand

        Parser.Read :: cmds ->
            { machine | status = Stdin }
                |> updateScript cmds


flush : BFMachine -> BFMachine
flush machine =
    { machine | stdout = [] }
