module Grid exposing (getNeighborCount, gridSideLength, initialGrid, randomGridGenerator, runSimulation, toggleCell)

import Random exposing (Generator)
import Random.Extra as RandomExtra
import SquareArray2D exposing (SquareArray2D)


{-| The side length of the square grid.
-}
gridSideLength : Int
gridSideLength =
    20


{-| The initial empty grid.
-}
initialGrid : SquareArray2D Bool
initialGrid =
    SquareArray2D.repeat gridSideLength False


{-| Toggles the cell at a certain coordinate in the grid.
-}
toggleCell : Int -> Int -> SquareArray2D Bool -> SquareArray2D Bool
toggleCell row column =
    SquareArray2D.update row column not


{-| Runs the Game of Life one iteration.
-}
runSimulation : SquareArray2D Bool -> SquareArray2D Bool
runSimulation array =
    array
        |> SquareArray2D.indexedMap
            (\row column cell ->
                let
                    neighbors : Int
                    neighbors =
                        getNeighborCount row column array
                in
                if neighbors < 2 || neighbors > 3 then
                    False

                else if not cell && neighbors == 3 then
                    True

                else
                    cell
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
getNeighborCount : Int -> Int -> SquareArray2D Bool -> Int
getNeighborCount i j array =
    operations
        |> List.filter (filterOperations i j array)
        |> List.length


{-| Filters `operations` to be only the cells that are alive around each cell.
-}
filterOperations : Int -> Int -> SquareArray2D Bool -> ( Int, Int ) -> Bool
filterOperations i j array ( a, b ) =
    array
        |> SquareArray2D.get (i + a) (j + b)
        |> Maybe.withDefault False


randomGridGenerator : Generator (SquareArray2D Bool)
randomGridGenerator =
    SquareArray2D.generator
        RandomExtra.bool
        gridSideLength
