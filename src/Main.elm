module Main exposing (main)

import Browser
import IDE exposing (Model, Msg, init, subscriptions, update, view)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
