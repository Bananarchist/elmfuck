module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Brainfuck as BF
import Browser
import Html exposing (..)


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
    }


type Msg
    = Input
    | Run
    | Stop
    | Cycle


init : () -> ( Model, Cmd Msg )
init _ =
    ( { machine = BF.createMachine
      , script = ""
      , running = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ textarea [] []
        , button [] [ text "Run" ]
        , code [] []
        , input [] []
        , button [] [ text "Kill" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
