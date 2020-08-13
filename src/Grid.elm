module Grid exposing (Grid, getNeighborCount, gridSideLength, initialGrid, randomGridGenerator, runSimulation, toggleCell)

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


{-| Generates a 'square' 2d array where every value is the same.
-}
squareArrayRepeat : Int -> a -> Array (Array a)
squareArrayRepeat length value =
    let
        arrayRepeat =
            Array.repeat length
    in
    value
        |> arrayRepeat
        |> arrayRepeat


{-| The initial empty grid.
-}
initialGrid : Grid
initialGrid =
    squareArrayRepeat gridSideLength False


{-| Generates a grid where each square is randomly filled in.
-}
randomGridGenerator : Generator Grid
randomGridGenerator =
    let
        randomArray =
            RandomArray.array gridSideLength
    in
    RandomExtra.bool
        |> randomArray
        |> randomArray


{-| Toggles the cell at a certain coordinate in the grid.
-}
toggleCell : Int -> Int -> Grid -> Grid
toggleCell a b =
    not
        |> ArrayExtra.update b
        |> ArrayExtra.update a


{-| Runs the Game of Life one iteration.
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
                                    getNeighborCount i j grid
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
getNeighborCount : Int -> Int -> Grid -> Int
getNeighborCount i j grid =
    operations
        |> List.filter (filterOperations i j grid)
        |> List.length


{-| Filters `operations` to be only the cells that are alive around each cell.
-}
filterOperations : Int -> Int -> Grid -> ( Int, Int ) -> Bool
filterOperations i j grid ( a, b ) =
    grid
        |> Array.get (i + a)
        |> Maybe.withDefault Array.empty
        |> Array.get (j + b)
        |> Maybe.withDefault False
