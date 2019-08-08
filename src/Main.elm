module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Time



-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------


type alias Model =
    { board : Board
    , generation : Int
    , pause : Bool
    , colorAlive : String
    , colorDead : String
    , colorBorder : String
    , refreshPeriod : Int
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


toggleCell : Coordinates -> Board -> Board
toggleCell coordinates board =
    let
        { state } =
            board
                |> getCell coordinates
                |> Maybe.withDefault (newCell Dead)

        newState =
            case state of
                Dead ->
                    Alive

                _ ->
                    Dead
    in
    setCell coordinates newState board


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
    , borderStyle = "1px solid"
    , cellSide = "10px"
    }


init : ( Model, Cmd Msg )
init =
    ( { board = initialBoard
      , generation = 0
      , pause = True
      , colorAlive = "#000000"
      , colorDead = "#ffffff"
      , colorBorder = "#bbbbbb"
      , refreshPeriod = 100
      }
    , Cmd.none
    )


initialBoard : Board
initialBoard =
    Array.repeat settings.height (Array.repeat settings.width (newCell Dead))



-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------


type Msg
    = Tick Time.Posix
    | TogglePause
    | InputColorAlive String
    | InputColorDead String
    | InputColorBorder String
    | InputRefreshPeriod Int
    | ToggleCell Coordinates


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

        TogglePause ->
            ( { model | pause = not model.pause }
            , Cmd.none
            )

        InputColorAlive value ->
            ( { model | colorAlive = value }
            , Cmd.none
            )

        InputColorDead value ->
            ( { model | colorDead = value }
            , Cmd.none
            )

        InputColorBorder value ->
            ( { model | colorBorder = value }
            , Cmd.none
            )

        InputRefreshPeriod value ->
            ( { model | refreshPeriod = value }
            , Cmd.none
            )

        ToggleCell coordinates ->
            ( { model | board = toggleCell coordinates model.board }
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
            , A.style "border-color" model.colorBorder
            ]
            (viewBoard model)
        , Html.p [] [ Html.text ("Generation: " ++ String.fromInt model.generation) ]
        , viewPauseButton model.pause
        , viewColorInput InputColorAlive model.colorAlive
        , viewColorInput InputColorDead model.colorDead
        , viewColorInput InputColorBorder model.colorBorder
        , viewRefreshPeriodInput model.refreshPeriod
        ]


viewBoard : Model -> List (Html Msg)
viewBoard model =
    let
        rowToCells y row =
            Html.tr []
                (row
                    |> Array.indexedMap (viewCell model y)
                    |> Array.toList
                )
    in
    model.board
        |> Array.indexedMap rowToCells
        |> Array.toList


viewCell : Model -> Int -> Int -> Cell -> Html Msg
viewCell model y x { state } =
    let
        color =
            case state of
                Dead ->
                    model.colorDead

                _ ->
                    model.colorAlive
    in
    Html.td
        [ A.style "background-color" color
        , A.style "border" settings.borderStyle
        , A.style "border-color" model.colorBorder
        , A.style "width" settings.cellSide
        , A.style "height" settings.cellSide
        , E.onClick (ToggleCell (Coordinates x y))
        ]
        []


viewPauseButton : Bool -> Html Msg
viewPauseButton pause =
    let
        text =
            case pause of
                True ->
                    "Play"

                False ->
                    "Pause"
    in
    Html.button [ E.onClick TogglePause ] [ Html.text text ]


viewColorInput : (String -> Msg) -> String -> Html Msg
viewColorInput toMsg color =
    Html.input
        [ A.type_ "color"
        , A.value color
        , E.onInput toMsg
        ]
        []


viewRefreshPeriodInput : Int -> Html Msg
viewRefreshPeriodInput refreshPeriod =
    Html.input
        [ A.type_ "range"
        , A.value (String.fromInt refreshPeriod)
        , A.min "50"
        , A.max "1000"
        , A.step "50"
        , E.onInput (InputRefreshPeriod << Maybe.withDefault refreshPeriod << String.toInt)
        ]
        []



-------------------------------------------------------------------------------
-- MAIN
-------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.pause of
        True ->
            Sub.none

        _ ->
            Time.every (toFloat model.refreshPeriod) Tick



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
