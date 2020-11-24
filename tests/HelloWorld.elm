module HelloWorld exposing (suite)

import Array as Array
import Brainfuck as BF
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Test Bf Interp"
        [ describe "machine.step"
            [ test "when waiting, stops advancing position until stdin is consumable" <|
                \_ ->
                    let
                        baseMachine =
                            BF.createMachine

                        machine =
                            BF.step { baseMachine | script = Array.fromList [ ',' ] }

                        machine2 =
                            BF.step { machine | stdin = [ 9 ] }
                    in
                    baseMachine
                        |> BF.step
                        |> .position
                        |> Expect.equal 1
            ]
        , describe "closingBracket locator"
            [ test "locates closing bracket without nested brackets" <|
                \_ ->
                    let
                        script =
                            "[->+<]and comments here"
                                |> String.toList
                                |> Array.fromList
                    in
                    BF.closingBracketForOpeningBracket script 0
                        |> Result.withDefault 0
                        |> Expect.equal 5
            , test "locates adjacent closing bracket" <|
                \_ ->
                    let
                        script =
                            "[]stuff" |> String.toList |> Array.fromList
                    in
                    BF.closingBracketForOpeningBracket script 0
                        |> Result.withDefault 0
                        |> Expect.equal 1
            , test "locates closing bracket with nested brackets" <|
                \_ ->
                    let
                        script =
                            "[[[]]]"
                                |> String.toList
                                |> Array.fromList
                    in
                    BF.closingBracketForOpeningBracket script 1
                        |> Result.withDefault 0
                        |> Expect.equal 4
            , test "returns Err trying to locate unmatched" <|
                \_ ->
                    let
                        script =
                            "[->+< and forget closing bracket because comments"
                                |> String.toList
                                |> Array.fromList
                    in
                    BF.closingBracketForOpeningBracket script 0
                        |> Result.withDefault 0
                        |> Expect.equal 0
            ]
        , todo "Implement Hello World: ++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
        ]
