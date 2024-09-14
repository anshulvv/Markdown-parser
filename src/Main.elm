module Main exposing (main)

import Browser
import Html exposing (Html)


type alias Model =
    { count : Int }


type Msg
    = Increment
    | Decrement


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init flags =
    ( { count = 0 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Title of the page"
    , body = []
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
