module Main exposing (main)

import Array
import Browser
import Grid exposing (gridSideLength, initialGrid, randomGridGenerator, runSimulation, toggleCell)
import Html exposing (Html, a, button, div, h3, input, label, li, p, text, ul)
import Html.Attributes exposing (class, classList, for, href, id, step, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy)
import Random
import SquareArray2D exposing (SquareArray2D)
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { grid : SquareArray2D Bool
    , started : Bool
    , simulationSpeed : Float
    }


initialModel : Model
initialModel =
    { grid = initialGrid
    , started = False
    , simulationSpeed = 750
    }


init : a -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )


type Msg
    = ToggleStarted
    | ChangeSimulationSpeed String
    | GetRandomizeGrid
    | RandomizedGrid (SquareArray2D Bool)
    | ResetGrid
    | RunSimulation
    | ToggleCell Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleStarted ->
            ( { model | started = not model.started }
            , Cmd.none
            )

        ChangeSimulationSpeed rangeString ->
            ( case String.toFloat rangeString of
                Just newSpeed ->
                    { model | simulationSpeed = newSpeed }

                Nothing ->
                    model
            , Cmd.none
            )

        GetRandomizeGrid ->
            ( { model | started = False }
            , Random.generate RandomizedGrid randomGridGenerator
            )

        RandomizedGrid newGrid ->
            ( { model | grid = newGrid }
            , Cmd.none
            )

        ResetGrid ->
            ( { model | grid = initialGrid, started = False }
            , Cmd.none
            )

        RunSimulation ->
            ( { model | grid = runSimulation model.grid }
            , Cmd.none
            )

        ToggleCell a b ->
            ( { model | grid = toggleCell a b model.grid }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.started then
        always RunSimulation
            |> Time.every model.simulationSpeed

    else
        Sub.none


simulationSpeedId : String
simulationSpeedId =
    "simul-speed"


gameOfLifeWikipediaLink : String
gameOfLifeWikipediaLink =
    "https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life"


startButton : Bool -> Html Msg
startButton started =
    button
        [ onClick ToggleStarted
        , classList
            [ ( "button", True )
            , ( if started then
                    "is-warning"

                else
                    "is-primary"
              , True
              )
            ]
        ]
        [ (if started then
            "Stop"

           else
            "Start"
          )
            |> text
        ]


simulationSpeedDisplayAndSlider : Float -> Html Msg
simulationSpeedDisplayAndSlider simulationSpeed =
    div
        [ class "field" ]
        [ label
            [ for simulationSpeedId
            , class "label"
            ]
            [ text <| "Time between simulation iterations: " ++ String.fromFloat simulationSpeed ++ " ms" ]
        , p
            [ class "control" ]
            [ input
                [ type_ "range"
                , class "slider"
                , step "50"
                , Html.Attributes.min "100"
                , Html.Attributes.max "2000"
                , id simulationSpeedId
                , value <| String.fromFloat simulationSpeed
                , onInput ChangeSimulationSpeed
                ]
                []
            ]
        ]


gridDisplay : SquareArray2D Bool -> Html Msg
gridDisplay grid =
    div
        [ style "display" "grid"
        , style "gridTemplateColumns" ("repeat(" ++ String.fromInt gridSideLength ++ ", 20px)")
        ]
        (grid
            |> SquareArray2D.indexedMap
                (\row column value ->
                    div
                        [ onClick <| ToggleCell row column
                        , classList
                            [ ( "cell", True )
                            , ( if value then
                                    "alive"

                                else
                                    "dead"
                              , True
                              )
                            ]
                        ]
                        []
                )
            |> SquareArray2D.toFlatArrayRowMajor
            |> Array.toList
        )


view : Model -> Html Msg
view model =
    div
        [ class "columns is-centered" ]
        [ div
            [ class "column is-narrow" ]
            [ h3
                [ class "title" ]
                [ text "Conway's Game of Life" ]
            , div
                [ class "content" ]
                [ p [] [ text "This is an implementation of Conway's Game of Life using Elm." ]
                , p [] [ text "Click a cell to toggle whether it is alive or dead." ]
                , p [] [ text "These are the rules of the game:" ]
                , ul []
                    [ li [] [ text "Any live cell with two or three live neighbours survives." ]
                    , li [] [ text "Any dead cell with three live neighbours becomes a live cell." ]
                    , li [] [ text "All other live cells die in the next generation. Similarly, all other dead cells stay dead." ]
                    ]
                , p []
                    [ text "Reference: "
                    , a
                        [ href gameOfLifeWikipediaLink ]
                        [ text gameOfLifeWikipediaLink ]
                    ]
                ]
            , div
                [ class "field is-grouped" ]
                [ p
                    [ class "control" ]
                    [ lazy startButton model.started ]
                , p
                    [ class "control" ]
                    [ button
                        [ onClick GetRandomizeGrid
                        , class "button is-info"
                        ]
                        [ text "Randomize" ]
                    ]
                , p
                    [ class "control" ]
                    [ button
                        [ onClick ResetGrid
                        , class "button is-danger"
                        ]
                        [ text "Reset" ]
                    ]
                ]
            , lazy simulationSpeedDisplayAndSlider model.simulationSpeed
            , lazy gridDisplay model.grid
            ]
        ]
