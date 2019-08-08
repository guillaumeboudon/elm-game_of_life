module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes as A



-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------


type alias Model =
    { board : Board }


type alias Board =
    Array (Array CellState)


setCell : { x : Int, y : Int } -> CellState -> Board -> Board
setCell { x, y } cellState board =
    let
        newRow =
            Array.get y board
                |> Maybe.withDefault Array.empty
                |> Array.set x cellState
    in
    board
        |> Array.set y newRow


type CellState
    = Dead
    | Alive


settings =
    { width = 21
    , height = 21
    , cellColorAlive = "#F00"
    , cellColorDead = "#000"
    , cellSide = "10px"
    }


init : Model
init =
    { board = initialBoard }


initialBoard : Board
initialBoard =
    let
        emptyBoard =
            Array.repeat settings.height (Array.repeat settings.width Dead)
    in
    emptyBoard
        |> setCell { x = 10, y = 10 } Alive
        |> setCell { x = 10, y = 8 } Alive
        |> setCell { x = 12, y = 10 } Alive
        |> setCell { x = 10, y = 12 } Alive
        |> setCell { x = 8, y = 10 } Alive



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
            Html.tr []
                (row
                    |> Array.map viewCell
                    |> Array.toList
                )
    in
    board
        |> Array.map rowToCells
        |> Array.toList


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
