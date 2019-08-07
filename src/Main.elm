module Main exposing (main)

import Browser
import Html exposing (Html)



-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------


type alias Model =
    { matrix : List (List Bool) }


init : Model
init =
    Model [ [] ]



-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-------------------------------------------------------------------------------
-- VIEW
-------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    Html.text "Conway's Game of Life"



-------------------------------------------------------------------------------
-- MAIN
-------------------------------------------------------------------------------


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
