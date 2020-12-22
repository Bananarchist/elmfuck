module BFParserTests exposing (suite)

import BFParser as BFP
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "individual token parsing"
        [ test "+" <|
            \_ ->
                "+++"
                    |> BFP.parse
                    |> Result.withDefault []
                    |> Expect.equalLists [ BFP.Byte 3 ]
        , test "-" <|
            \_ ->
                "----"
                    |> BFP.parse
                    |> Result.withDefault []
                    |> Expect.equalLists [ BFP.Byte -4 ]
        , test "<" <|
            \_ ->
                "<<<<<"
                    |> BFP.parse
                    |> Result.withDefault []
                    |> Expect.equalLists [ BFP.Ptr -5 ]
        , test ">" <|
            \_ ->
                ">>>>>>"
                    |> BFP.parse
                    |> Result.withDefault []
                    |> Expect.equalLists [ BFP.Ptr 6 ]
        , test "." <|
            \_ ->
                "."
                    |> BFP.parse
                    |> Result.withDefault []
                    |> Expect.equalLists [ BFP.Print ]
        , test "," <|
            \_ ->
                ","
                    |> BFP.parse
                    |> Result.withDefault []
                    |> Expect.equalLists [ BFP.Read ]
        , test "[]" <|
            \_ ->
                "[+]"
                    |> BFP.parse
                    |> Result.withDefault []
                    |> Expect.equalLists [ BFP.Block [ BFP.Byte 1 ] ]
        , test "[hello this is an ignored comment+]" <|
            \_ ->
                "[hello this is an ignored comment+]"
                    |> BFP.parse
                    |> Result.withDefault []
                    |> Expect.equalLists [ BFP.Block [ BFP.Byte 1 ] ]
        ]
