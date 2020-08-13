module Tests exposing (suite)

import Expect
import Grid exposing (getNeighborCount, initialGrid, runSimulation, toggleCell)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Game of Life test suite"
        [ gridTests
        ]


gridTests : Test
gridTests =
    describe "The Grid module"
        [ describe "toggleCell"
            [ test "Does nothing when the index is out of bounds" <|
                \_ ->
                    initialGrid
                        |> toggleCell -1 -1
                        |> Expect.equal initialGrid
            , test "Turning on and off a cell returns the grid to how it was before" <|
                \_ ->
                    let
                        toggleFunc =
                            toggleCell 1 1
                    in
                    initialGrid
                        |> toggleFunc
                        |> toggleFunc
                        |> Expect.equal initialGrid
            ]
        , describe "getNeighborCount"
            [ test "Returns 8 for a completely surrounded cell" <|
                \_ ->
                    initialGrid
                        |> toggleCell 0 0
                        |> toggleCell 0 1
                        |> toggleCell 0 2
                        |> toggleCell 1 0
                        |> toggleCell 1 1
                        |> toggleCell 1 2
                        |> toggleCell 2 0
                        |> toggleCell 2 1
                        |> toggleCell 2 2
                        |> getNeighborCount 1 1
                        |> Expect.equal 8
            ]
        , describe "runSimulation"
            [ test "Does nothing on an empty grid" <|
                \_ ->
                    initialGrid
                        |> runSimulation
                        |> Expect.equal initialGrid
            , test "If only one cell is alive then it will die" <|
                \_ ->
                    initialGrid
                        |> toggleCell 5 5
                        |> runSimulation
                        |> Expect.equal initialGrid
            , test "A square will stay alive and not move" <|
                \_ ->
                    let
                        gridWithSquare =
                            initialGrid
                                |> toggleCell 5 5
                                |> toggleCell 5 6
                                |> toggleCell 6 5
                                |> toggleCell 6 6
                    in
                    gridWithSquare
                        |> runSimulation
                        |> Expect.equal gridWithSquare
            ]
        ]
