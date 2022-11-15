module BFParserTests exposing (suite)

import BFParser as BFP
import Expect exposing (Expectation)
import Test exposing (..)
import Fuzz

bfop : Fuzz.Fuzzer BFP.BFOP
bfop =
    Fuzz.oneOf
        [ Fuzz.map BFP.Byte Fuzz.int
        , Fuzz.map BFP.Ptr Fuzz.int
        , Fuzz.constant BFP.Print 
        , Fuzz.constant BFP.Read
        ]

bfopBlock : Int -> Fuzz.Fuzzer BFP.BFOP
bfopBlock depth =
    let 
        opts = 
            if depth <= 0 then
                [ bfop ]
            else
                [ bfop, bfopBlock (depth - 1) ]
    in
    Fuzz.map BFP.Block (Fuzz.list (Fuzz.oneOf opts))


script : Fuzz.Fuzzer (List BFP.BFOP)
script =
    Fuzz.list (Fuzz.oneOf [ bfop, bfopBlock 1 ])
    

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
