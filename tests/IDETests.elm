module IDETests exposing (suite)

import Expect
import Fuzz
import IDE
import Main
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Q
import Test.Html.Selector as Select


{-| helpers for discarding CMD outputs
-}
update : IDE.Msg -> IDE.Model -> IDE.Model
update msg =
    IDE.update msg >> Tuple.first


updates : List IDE.Msg -> IDE.Model -> IDE.Model
updates msgs model =
    List.foldl update model msgs


initializeToState : List IDE.Msg -> IDE.Model
initializeToState msgs =
    IDE.init ()
        |> Tuple.first
        |> updates msgs


suite : Test
suite =
    [ test "SumOf2And5 script outputs '7'" <|
        \_ ->
            initializeToState [ IDE.LoadScript IDE.SumOf2And5, IDE.Run ]
                |> IDE.view
                |> Q.fromHtml
                |> Q.find [ Select.class "output" ]
                |> Q.has [ Select.text "7" ]
    , test "HelloWorld script outputs 'Hello World!'" <|
        \_ ->
            initializeToState [ IDE.LoadScript IDE.HelloWorld, IDE.Run ]
                |> IDE.view
                |> Q.fromHtml
                |> Q.find [ Select.class "output" ]
                |> Q.has [ Select.text "Hello World!" ]
    , test "Takes input and offsets ASCII 2 to print" <|
        \_ ->
            initializeToState [ IDE.UpdateScript ",++.", IDE.Run, IDE.Stdin "a" ]
                |> IDE.view
                |> Q.fromHtml
                |> Q.findAll [ Select.class "output" ]
                |> Q.index -1
                |> Q.has [ Select.text "c" ]
    , test "Removes inputs when out of sequence" <|
        \_ ->
            initializeToState [ IDE.UpdateScript ",++.", IDE.Run, IDE.Stdin "a" ]
                |> IDE.view
                |> Q.fromHtml
                |> Q.findAll [ Select.class "input" ]
                |> Q.count (Expect.equal 0)
    ]
        |> describe "IDE Features"

