module Grid exposing (Grid, gridSideLength, initialGrid, randomGridGenerator, runSimulation, toggleCell)

import Array exposing (Array)
import Array.Extra as ArrayExtra
import Random exposing (Generator)
import Random.Array as RandomArray
import Random.Extra as RandomExtra


{-| The side length of the square grid.
-}
gridSideLength : Int
gridSideLength =
    20


{-| A 2 dimensional array representing the grid for Conway's Game of Life.
-}
type alias Grid =
    Array (Array Bool)


{-| Toggles the cell at a certain coordinate in the grid.
-}
toggleCell : Int -> Int -> Grid -> Grid
toggleCell a b =
    ArrayExtra.update a <| ArrayExtra.update b not


{-| Generates a 'square' 2d array where every value is the same.
-}
squareArrayRepeat : Int -> a -> Array (Array a)
squareArrayRepeat length value =
    value
        |> Array.repeat length
        |> Array.repeat length


{-| The initial empty grid.
-}
initialGrid : Grid
initialGrid =
    squareArrayRepeat gridSideLength False


{-| Generates a grid where each square is randomly filled in.
-}
randomGridGenerator : Generator Grid
randomGridGenerator =
    RandomExtra.bool
        |> RandomArray.array gridSideLength
        |> RandomArray.array gridSideLength


{-| Runs the Game of Life once.
-}
runSimulation : Grid -> Grid
runSimulation grid =
    grid
        |> Array.indexedMap
            (\i row ->
                row
                    |> Array.indexedMap
                        (\j col ->
                            let
                                neighbors =
                                    getNeighborCount grid i j
                            in
                            if neighbors < 2 || neighbors > 3 then
                                False

                            else if not col && neighbors == 3 then
                                True

                            else
                                col
                        )
            )


{-| A list of all the possible neighbors a cell can have.
-}
operations : List ( Int, Int )
operations =
    [ ( 0, 1 )
    , ( 0, -1 )
    , ( 1, 1 )
    , ( 1, 0 )
    , ( 1, -1 )
    , ( -1, 1 )
    , ( -1, 0 )
    , ( -1, -1 )
    ]


{-| Counts the number of neighbors that a certain cell has.
-}
getNeighborCount : Grid -> Int -> Int -> Int
getNeighborCount grid i j =
    operations
        |> List.filter (filterOperations grid i j)
        |> List.length


{-| Filters `operations` to be only the cells that are alive around each cell.
-}
filterOperations : Grid -> Int -> Int -> ( Int, Int ) -> Bool
filterOperations grid i j ( a, b ) =
    grid
        |> Array.get (i + a)
        |> Maybe.withDefault Array.empty
        |> Array.get (j + b)
        |> Maybe.withDefault False
