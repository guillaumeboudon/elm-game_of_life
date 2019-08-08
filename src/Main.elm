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
    , generation : Int
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


getCell : Coordinates -> Board -> Maybe Cell
getCell { x, y } board =
    board
        |> Array.get y
        |> Maybe.andThen (Array.get x)


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
    { width = 51
    , height = 51
    , borderStyle = "1px solid #bbb"
    , cellColorAlive = "#000"
    , cellColorDead = "#fff"
    , cellSide = "10px"
    }


init : ( Model, Cmd Msg )
init =
    ( { board = initialBoard
      , generation = 0
      }
    , Cmd.none
    )


initialBoard : Board
initialBoard =
    let
        emptyBoard =
            Array.repeat settings.height (Array.repeat settings.width (newCell Dead))
    in
    emptyBoard
        |> setCell (Coordinates 0 0) Alive
        |> setCell (Coordinates 1 1) Alive
        |> setCell (Coordinates 2 1) Alive
        |> setCell (Coordinates 0 2) Alive
        |> setCell (Coordinates 1 2) Alive
        |> setCell (Coordinates 15 2) Alive
        |> setCell (Coordinates 16 2) Alive
        |> setCell (Coordinates 17 2) Alive



-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------


type Msg
    = Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model
                | board = processNeighbours model.board
                , generation = model.generation + 1
              }
            , Cmd.none
            )


processNeighbours : Board -> Board
processNeighbours board =
    board
        |> Array.indexedMap (processRow board)
        |> applyRules


processRow : Board -> Int -> Array Cell -> Array Cell
processRow board y row =
    Array.indexedMap (processCell board y) row


processCell : Board -> Int -> Int -> Cell -> Cell
processCell board y x cell =
    let
        checkNeighbour x_ y_ cell_ =
            case getCell (Coordinates x_ y_) board of
                Nothing ->
                    cell_

                Just neighbour ->
                    case neighbour.state of
                        Dead ->
                            cell_

                        _ ->
                            { cell_
                                | neighbours =
                                    case cell_.neighbours of
                                        Nothing ->
                                            Just 1

                                        Just n ->
                                            Just (n + 1)
                            }
    in
    cell
        |> checkNeighbour (x - 1) (y - 1)
        |> checkNeighbour x (y - 1)
        |> checkNeighbour (x + 1) (y - 1)
        |> checkNeighbour (x - 1) y
        |> checkNeighbour (x + 1) y
        |> checkNeighbour (x - 1) (y + 1)
        |> checkNeighbour x (y + 1)
        |> checkNeighbour (x + 1) (y + 1)


applyRules : Board -> Board
applyRules board =
    let
        apply row =
            Array.map applyRule row
    in
    Array.map apply board


applyRule : Cell -> Cell
applyRule { state, neighbours } =
    case ( state, neighbours ) of
        ( Dead, Just 3 ) ->
            newCell Alive

        ( Alive, Just 2 ) ->
            newCell Alive

        ( Alive, Just 3 ) ->
            newCell Alive

        _ ->
            newCell Dead



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
        , Html.p [] [ Html.text ("Generation: " ++ String.fromInt model.generation) ]
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
    Time.every 100 Tick



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
