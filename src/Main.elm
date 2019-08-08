module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes as A
import Time



-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------


type alias Model =
    { board : Board
    }


type alias Board =
    Array (Array Cell)


type alias Coordinates =
    { x : Int
    , y : Int
    }


setCell : Coordinates -> CellState -> Board -> Board
setCell { x, y } state board =
    let
        newRow =
            Array.get y board
                |> Maybe.withDefault Array.empty
                |> Array.set x (newCell state)
    in
    board
        |> Array.set y newRow


type CellState
    = Dead
    | Alive


type alias Cell =
    { state : CellState
    , neighbours : Maybe Int
    }


newCell : CellState -> Cell
newCell state =
    Cell state Nothing


settings =
    { width = 21
    , height = 21
    , borderStyle = "1px solid #bbb"
    , cellColorAlive = "#000"
    , cellColorDead = "#fff"
    , cellSide = "10px"
    }


init : ( Model, Cmd Msg )
init =
    ( { board = initialBoard }
    , Cmd.none
    )


initialBoard : Board
initialBoard =
    let
        emptyBoard =
            Array.repeat settings.height (Array.repeat settings.width (newCell Dead))
    in
    emptyBoard
        |> setCell (Coordinates 10 10) Alive
        |> setCell (Coordinates 10 8) Alive
        |> setCell (Coordinates 12 10) Alive
        |> setCell (Coordinates 10 12) Alive
        |> setCell (Coordinates 8 10) Alive



-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------


type Msg
    = Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( model
            , Cmd.none
            )



-------------------------------------------------------------------------------
-- VIEW
-------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Conway's Game of Life" ]
        , Html.table
            [ A.style "border-collapse" "collapse"
            , A.style "border" settings.borderStyle
            ]
            (viewBoard model.board)
        ]


viewBoard : Board -> List (Html Msg)
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


viewCell : Cell -> Html Msg
viewCell { state } =
    let
        color =
            case state of
                Dead ->
                    settings.cellColorDead

                _ ->
                    settings.cellColorAlive
    in
    Html.td
        [ A.style "background-color" color
        , A.style "border" settings.borderStyle
        , A.style "width" settings.cellSide
        , A.style "height" settings.cellSide
        ]
        []



-------------------------------------------------------------------------------
-- MAIN
-------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-------------------------------------------------------------------------------
-- MAIN
-------------------------------------------------------------------------------


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
