module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Brainfuck as BF
import Browser
import Html exposing (..)
import Html.Attributes as Hats
import Html.Events as Hevs


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
    , console : String
    }


type Msg
    = UpdateScript String
    | Run
    | Stop
    | Cycle


init : () -> ( Model, Cmd Msg )
init _ =
    ( { machine = BF.createMachine
      , script = ""
      , running = False
      , console = ""
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            ( { model
                | machine = BF.initMachine model.script
                , running = True
              }
            , Cmd.none
            )

        Stop ->
            ( { model | running = False }
            , Cmd.none
            )

        Cycle ->
            let
                machine =
                    if model.running then
                        BF.step model.machine

                    else
                        model.machine
            in
            ( { model | machine = machine }
            , Cmd.none
            )

        UpdateScript s ->
            ( { model | script = s }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        runAttrs =
            if model.running then
                [ Hevs.onClick Stop ]

            else
                [ Hevs.onClick Run ]

        runText =
            if model.running then
                "Kill"

            else
                "Run"
    in
    main_ []
        [ textarea [ Hevs.onInput UpdateScript ] [ text model.script ]
        , button runAttrs [ text runText ]
        , code [ Hats.contenteditable True ] [ text model.console ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
