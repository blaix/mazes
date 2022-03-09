module Main exposing (main)

import Browser
import Dict exposing (update)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , el
        , height
        , layout
        , px
        , row
        , text
        , width
        )
import Element.Border as Border
import Html exposing (Html)
import List.Extra exposing (getAt, setAt)
import Random
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


{-| A 2-dimensional grid of cells
-}
type alias Model =
    { grid : Grid
    , sizeX : Int
    , sizeY : Int
    , cellSize : Int
    }


type alias Grid =
    List (List Cell)


{-| Cells have a north and east wall.

    _
     |

They can be arranged in a grid like this:

    _ _ _
    _|_|_|
    _|_|_|
     | | |

Then you can give the grid its own south and west wall:

     _ _ _
    |_|_|_|
    |_|_|_|
    |_|_|_|

This forms the template for our maze.
To turn it into a maze, we carve through it one cell at a time,
removing either an eastern or northern wall.
Ending up with something like this (for example):

     _ _ _
    |_    |
    |  _| |
    |_|_ _|

The data structure consists of two bools showing if the north and east walls exist.

-}
type alias Cell =
    { north : Bool
    , east : Bool
    }


type Direction
    = North
    | East


{-| Create the maze and start carving in the top left cell.
-}
init : flags -> ( Model, Cmd Msg )
init _ =
    let
        sizeX =
            15

        sizeY =
            10
    in
    ( { grid = List.repeat sizeY (List.repeat sizeX (Cell True True))
      , sizeX = sizeX
      , sizeY = sizeY
      , cellSize = 50
      }
    , carvePathCmd ( 0, 0 )
    )



-- UPDATE


type Msg
    = CarvePath ( Int, Int ) -- Start (or continue) carving a path at the specified point
    | RemoveWall ( Int, Int ) Direction -- Remove a wall at the specified point


carvePathCmd : ( Int, Int ) -> Cmd Msg
carvePathCmd ( x, y ) =
    Task.perform CarvePath (Task.succeed ( x, y ))


removeWallCmd : ( Int, Int ) -> Direction -> Cmd Msg
removeWallCmd ( x, y ) direction =
    Task.perform (RemoveWall ( x, y )) (Task.succeed direction)


randomDirection : Random.Generator Direction
randomDirection =
    Random.uniform North [ East ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CarvePath ( x, y ) ->
            ( model, model |> carveWithBinaryTree ( x, y ) )

        RemoveWall ( x, y ) direction ->
            ( model |> removeWall ( x, y ) direction
            , carvePathCmd ( x + 1, y )
            )


{-| Carve a path using the binary tree maze algorithm.
<http://weblog.jamisbuck.org/2011/2/1/maze-generation-binary-tree-algorithm>
-}
carveWithBinaryTree : ( Int, Int ) -> Model -> Cmd Msg
carveWithBinaryTree ( x, y ) model =
    if y >= model.sizeY then
        -- Carved every row. All done!
        Cmd.none

    else if x >= model.sizeX then
        -- Reached the end of this row, drop down to next one
        carvePathCmd ( 0, y + 1 )

    else if northeastCorner ( x, y ) model then
        -- Special case: Don't remove any wall in northeast corner.
        carvePathCmd ( 0, 1 )

    else if y == 0 then
        -- Don't remove the northern maze boundary
        removeWallCmd ( x, y ) East

    else if x == model.sizeX - 1 then
        -- Don't remove the eastern maze boundary
        removeWallCmd ( x, y ) North

    else
        -- Otherwise remove a random north or east wall!
        Random.generate (RemoveWall ( x, y )) randomDirection


northeastCorner : ( Int, Int ) -> Model -> Bool
northeastCorner ( x, y ) model =
    (y == 0) && (x == model.sizeX - 1)


removeWall : ( Int, Int ) -> Direction -> Model -> Model
removeWall ( x, y ) direction model =
    let
        maybeRow =
            getAt y model.grid

        maybeCell =
            getAt x (Maybe.withDefault [] maybeRow)
    in
    case ( maybeRow, maybeCell ) of
        ( Just row, Just cell ) ->
            let
                newCell =
                    case direction of
                        North ->
                            { cell | north = False }

                        East ->
                            { cell | east = False }

                newRow =
                    row |> setAt x newCell
            in
            { model | grid = setAt y newRow model.grid }

        -- If we got a point that's out of bounds there's nothing we can do
        _ ->
            model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    layout [] <|
        column
            [ centerX
            , centerY
            , Border.widthEach
                { top = 0
                , right = 0
                , bottom = 1
                , left = 1
                }
            ]
            (List.map drawRow model.grid)


drawRow : List Cell -> Element Msg
drawRow cells =
    row [] (List.map drawCell cells)


drawCell : Cell -> Element Msg
drawCell cell =
    let
        borderTop =
            if cell.north then
                1

            else
                0

        borderRight =
            if cell.east then
                1

            else
                0
    in
    el
        [ width (px 50)
        , height (px 50)
        , Border.widthEach
            { top = borderTop
            , right = borderRight
            , bottom = 0
            , left = 0
            }
        ]
        (text " ")
