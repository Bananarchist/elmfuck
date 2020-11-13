module HelloWorld exposing (suite)

import Array as Array
import Expect exposing (Expectation)
import Main as BF
import Test exposing (..)


suite : Test
suite =
    describe "Test machine"
        [ describe "machine.cycle"
            [ test "when waiting, stops advancing position until stdin is consumable" <|
                \_ ->
                    let
                        baseMachine =
                            BF.createMachine

                        machine =
                            BF.cycle { baseMachine | script = Array.fromList [ ',' ] }

                        machine2 =
                            BF.cycle { machine | stdin = Array.fromList [ 9 ] }
                    in
                    baseMachine
                        |> BF.cycle
                        |> .position
                        |> Expect.equal 1
            ]
        , todo "Implement Hello World: ++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
        ]
