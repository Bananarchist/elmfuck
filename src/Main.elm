module Main exposing (main)

import Brainfuck as BF
import Browser
import Html exposing (..)
import Html.Attributes as Hats
import Html.Events as Emit
import Stack exposing (Stack)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { machine : BF.BFMachine
    , script : String
    , running : Bool
    , console : Stack ConsoleLine
    }


type Msg
    = UpdateScript String
    | Run
    | Stop
    | Stdin String
    | LoadScript SavedScript


type SavedScript
    = HelloWorld
    | SumOf2And5


type ConsoleLine
    = Output String
    | InputLine String
    | Error String
    | RunSuccess


flip : (a -> b -> c) -> b -> a -> c
flip fn p2 p1 =
    fn p1 p2


duple : a -> ( a, a )
duple a =
    ( a, a )


appendOutput : List Char -> Stack ConsoleLine -> Stack ConsoleLine
appendOutput addendum console =
    case Stack.peak console of
        Ok (Output str) ->
            Stack.pop console
                |> Stack.push (Output <| str ++ String.fromList addendum)

        _ ->
            Stack.push (Output <| String.fromList addendum) console


appendInput : String -> Stack ConsoleLine -> Stack ConsoleLine
appendInput addendum console =
    case Stack.peak console of
        Ok (InputLine str) ->
            Stack.pop console
                |> Stack.push (InputLine <| str ++ addendum)

        _ ->
            Stack.push (InputLine addendum) console


appendError : String -> Stack ConsoleLine -> Stack ConsoleLine
appendError addendum =
    Stack.push (Error addendum)


appendSuccess : Stack ConsoleLine -> Stack ConsoleLine
appendSuccess =
    Stack.push RunSuccess


init : () -> ( Model, Cmd Msg )
init _ =
    ( { machine = BF.createMachine
      , script = ""
      , running = False
      , console = Stack.new
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadScript scr ->
            ( { model | script = scriptFor scr }
            , Cmd.none
            )

        Run ->
            let
                machine =
                    BF.initMachine model.script
                        |> BF.runMachine

                updated =
                    { model | machine = machine }
            in
            case machine.status of
                BF.Stdin ->
                    case Stack.peak model.console of
                        Ok (InputLine _) ->
                            -- don't update last line because it is input line
                            ( updated
                            , Cmd.none
                            )

                        _ ->
                            ( { updated
                                | console = Stack.push (InputLine "") model.console
                              }
                            , Cmd.none
                            )

                -- this one needs to be advanced somehow...
                BF.Complete (Ok ()) ->
                    ( { updated | console = appendOutput machine.stdout model.console |> appendSuccess }, Cmd.none )

                BF.Complete (Err str) ->
                    ( { updated | console = appendError str model.console }, Cmd.none )

                _ ->
                    ( updated, Cmd.none )

        Stdin input ->
            let
                chr =
                    String.uncons input
                        |> Maybe.map Tuple.first

                ( console, machine ) =
                    chr
                        |> Maybe.map duple
                        |> Maybe.map
                            (Tuple.mapBoth
                                (String.fromChar >> flip appendInput model.console)
                                (BF.input model.machine >> BF.flush >> BF.runMachine)
                            )
                        |> Maybe.withDefault ( model.console, model.machine )
            in
            ( { model | machine = machine, console = console }, Cmd.none )

        Stop ->
            ( { model | running = False }
            , Cmd.none
            )

        UpdateScript s ->
            ( { model | script = s }
            , Cmd.none
            )


view : Model -> Html Msg
view =
    List.repeat 2
        >> List.map2 (<|) [ viewScript, viewConsole ]
        >> main_ []


viewScript : Model -> Html Msg
viewScript model =
    let
        runAttrs =
            if model.running then
                [ Emit.onClick Stop ]

            else
                [ Emit.onClick Run ]

        runText =
            if model.running then
                "Kill"

            else
                "Run"
    in
    section []
        [ textarea [ Emit.onInput UpdateScript, Hats.value model.script ] []
        , button [ Emit.onClick (LoadScript HelloWorld) ] [ text "Load HelloWorld" ]
        , button [ Emit.onClick (LoadScript SumOf2And5) ] [ text "Load SumOf2And5" ]
        , button runAttrs [ text runText ]
        ]


viewConsole : Model -> Html Msg
viewConsole =
    .console
        >> Stack.toList
        >> List.reverse
        >> List.map viewConsoleLine
        >> section []


viewConsoleLine : ConsoleLine -> Html Msg
viewConsoleLine line =
    case line of
        Error string ->
            p [ Hats.class "error" ] [ text string ]

        Output string ->
            p [ Hats.class "output" ] [ text string ]

        InputLine string ->
            p [ Hats.class "input" ]
                [ text string
                , input [ Hats.type_ "text", Emit.onInput Stdin, Hats.size 1 ] []
                ]

        RunSuccess ->
            p [ Hats.class "success" ] [ text "Done!" ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--| These are all from wikipedia


scriptFor : SavedScript -> String
scriptFor ss =
    case ss of
        HelloWorld ->
            """
++++++++               Set Cell #0 to 8
[
    >++++               Add 4 to Cell #1; this will always set Cell #1 to 4
    [                   as the cell will be cleared by the loop
        >++             Add 2 to Cell #2
        >+++            Add 3 to Cell #3
        >+++            Add 3 to Cell #4
        >+              Add 1 to Cell #5
        <<<<-           Decrement the loop counter in Cell #1
    ]                   Loop until Cell #1 is zero; number of iterations is 4
    >+                  Add 1 to Cell #2
    >+                  Add 1 to Cell #3
    >-                  Subtract 1 from Cell #4
    >>+                 Add 1 to Cell #6
    [<]                 Move back to the first zero cell you find; this will
                        be Cell #1 which was cleared by the previous loop
    <-                  Decrement the loop Counter in Cell #0
]                       Loop until Cell #0 is zero; number of iterations is 8

The result of this is:
Cell no :   0   1   2   3   4   5   6
Contents:   0   0  72 104  88  32   8
Pointer :   ^

>>.                     Cell #2 has value 72 which is 'H'
>---.                   Subtract 3 from Cell #3 to get 101 which is 'e'
+++++++..+++.           Likewise for 'llo' from Cell #3
>>.                     Cell #5 is 32 for the space
<-.                     Subtract 1 from Cell #4 for 87 to give a 'W'
<.                      Cell #3 was set to 'o' from the end of 'Hello'
+++.------.--------.    Cell #3 for 'rl' and 'd'
>>+.                    Add 1 to Cell #5 gives us an exclamation point
>++.                    And finally a newline from Cell #6
"""

        SumOf2And5 ->
            """
++       Cell c0 = 2
> +++++  Cell c1 = 5

[        Start your loops with your cell pointer on the loop counter (c1 in our case)
< +      Add 1 to c0
> -      Subtract 1 from c1
]        End your loops with the cell pointer on the loop counter

At this point our program has added 5 to 2 leaving 7 in c0 and 0 in c1
but we cannot output this value to the terminal since it is not ASCII encoded

To display the ASCII character "7" we must add 48 to the value 7
We use a loop to compute 48 = 6 * 8

++++ ++++  c1 = 8 and this will be our loop counter again
[
< +++ +++  Add 6 to c0
> -        Subtract 1 from c1
]
< .        Print out c0 which has the value 55 which translates to "7"!
"""
