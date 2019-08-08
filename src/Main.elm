module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as A



-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------


type alias Model =
    { board : Board }


type alias Board =
    List (List CellState)


type CellState
    = Dead
    | Alive


settings =
    { width = 20
    , height = 20
    , cellColorAlive = "#F00"
    , cellColorDead = "#000"
    , cellSide = "10px"
    }


init : Model
init =
    { board = initialBoard }


initialBoard : Board
initialBoard =
    List.repeat settings.height (List.repeat settings.width Dead)



-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------


update : msg -> Model -> Model
update _ model =
    model



-------------------------------------------------------------------------------
-- VIEW
-------------------------------------------------------------------------------


view : Model -> Html msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Conway's Game of Life" ]
        , Html.table
            [ A.style "border-spacing" "0" ]
            (viewBoard model.board)
        ]


viewBoard : Board -> List (Html msg)
viewBoard board =
    let
        rowToCells row =
            Html.tr [] (List.map viewCell row)
    in
    List.map rowToCells board


viewCell : CellState -> Html msg
viewCell bool =
    let
        color =
            case bool of
                Dead ->
                    settings.cellColorDead

                _ ->
                    settings.cellColorAlive
    in
    Html.td
        [ A.style "background-color" color
        , A.style "width" settings.cellSide
        , A.style "height" settings.cellSide
        ]
        []



-------------------------------------------------------------------------------
-- MAIN
-------------------------------------------------------------------------------


main : Program () Model msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
