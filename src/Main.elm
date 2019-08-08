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
    , ticTac : String
    }


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
    , borderStyle = "1px solid #bbb"
    , cellColorAlive = "#000"
    , cellColorDead = "#fff"
    , cellSide = "10px"
    }


init : ( Model, Cmd Msg )
init =
    ( { board = initialBoard
      , ticTac = "tic"
      }
    , Cmd.none
    )


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


type Msg
    = Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                newTicTac =
                    case model.ticTac of
                        "tic" ->
                            "tac"

                        _ ->
                            "tic"
            in
            ( { model | ticTac = newTicTac }
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
        , Html.pre [] [ Html.text model.ticTac ]
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


viewCell : CellState -> Html Msg
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
