module Main exposing (main)

import Browser
import Element
    exposing
        ( Element
        , alignTop
        , column
        , el
        , height
        , layout
        , padding
        , paddingXY
        , px
        , rgb255
        , row
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import List exposing (range)
import List.Extra exposing (getAt, setAt)
import Random
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init initialMazeSize initialCellSize
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
    , currentPosition : Position
    }


type alias Grid =
    List (List Cell)


type alias Position =
    ( Int, Int )


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


initialMazeSize : Int
initialMazeSize =
    40


initialCellSize : Int
initialCellSize =
    20


{-| Create the maze and start carving in the top left cell.
-}
init : Int -> Int -> flags -> ( Model, Cmd Msg )
init mazeSize cellSize _ =
    let
        sizeX =
            mazeSize

        sizeY =
            round (toFloat mazeSize * (4 / 6))
    in
    ( { grid = List.repeat sizeY (List.repeat sizeX (Cell True True))
      , sizeX = sizeX
      , sizeY = sizeY
      , cellSize = cellSize
      , currentPosition = ( 0, 0 )
      }
    , carvePathCmd
    )



-- UPDATE


type Msg
    = CarvePath -- Start (or continue) carving a path at the specified point
    | RemoveWall Direction -- Remove a wall at the specified point
    | ChangeMazeSize Int
    | ChangeCellSize Int


carvePathCmd : Cmd Msg
carvePathCmd =
    Task.perform (\_ -> CarvePath) (Task.succeed ())


removeWallCmd : Direction -> Cmd Msg
removeWallCmd direction =
    Task.perform RemoveWall (Task.succeed direction)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CarvePath ->
            carvePath model

        RemoveWall direction ->
            removeWall model direction

        ChangeMazeSize newSize ->
            init newSize model.cellSize ()

        ChangeCellSize newSize ->
            init model.sizeX newSize ()


carvePath : Model -> ( Model, Cmd Msg )
carvePath model =
    let
        ( x, y ) =
            model.currentPosition
    in
    if y >= model.sizeY then
        -- Carved every row. All done!
        ( model, Cmd.none )

    else if (x >= model.sizeX) || northeastCorner model then
        -- Reached the end of this row, drop down to next one
        ( { model | currentPosition = ( 0, y + 1 ) }, carvePathCmd )

    else if y == 0 then
        -- Can only remove the eastern wall if we're on the top row
        ( model, removeWallCmd East )

    else if x == model.sizeX - 1 then
        -- Can only remove the northern wall if we're on the east-most side
        ( model, removeWallCmd North )

    else
        -- Otherwise remove a random north or east wall!
        ( model, Random.generate RemoveWall randomDirection )


northeastCorner : Model -> Bool
northeastCorner model =
    let
        ( x, y ) =
            model.currentPosition
    in
    (y == 0) && (x == model.sizeX - 1)


randomDirection : Random.Generator Direction
randomDirection =
    Random.uniform North [ East ]


removeWall : Model -> Direction -> ( Model, Cmd Msg )
removeWall model direction =
    let
        ( x, y ) =
            model.currentPosition

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
            ( { model
                | grid = setAt y newRow model.grid
                , currentPosition = ( x + 1, y )
              }
            , carvePathCmd
            )

        -- If we got a point that's out of bounds there's nothing we can do
        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    layout [] <|
        column [ padding 20 ]
            [ row [ paddingXY 0 5 ] [ text "Algorithm: Binary Tree (TODO: algorithm selection!)" ]
            , row [ Font.size 18 ] [ text "Fast but makes sucky mazes (paths skew up-and-right)" ]
            , row [ paddingXY 0 20 ]
                [ column [ padding 20, alignTop ]
                    [ row []
                        [ slider
                            { label = "Maze Size: " ++ String.fromInt model.sizeX
                            , onChange = ChangeMazeSize
                            , model = model
                            , field = .sizeX
                            , range = ( 3, 80 )
                            }
                        ]
                    , row []
                        [ slider
                            { label = "Path Size: " ++ String.fromInt model.cellSize
                            , onChange = ChangeCellSize
                            , model = model
                            , field = .cellSize
                            , range = ( 3, 100 )
                            }
                        ]
                    ]
                , column
                    [ Border.widthEach
                        { top = 0
                        , right = 0
                        , bottom = 1
                        , left = 1
                        }
                    ]
                    (List.map (drawRow model.cellSize) model.grid)
                ]
            ]


drawRow : Int -> List Cell -> Element Msg
drawRow cellSize cells =
    row [] (List.map (drawCell cellSize) cells)


drawCell : Int -> Cell -> Element Msg
drawCell size cell =
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
        [ width (px size)
        , height (px size)
        , Border.widthEach
            { top = borderTop
            , right = borderRight
            , bottom = 0
            , left = 0
            }
        ]
        (text " ")


slider :
    { label : String
    , onChange : Int -> Msg
    , model : Model
    , field : Model -> Int
    , range : ( Int, Int )
    }
    -> Element Msg
slider { label, onChange, model, field, range } =
    let
        ( min, max ) =
            range
    in
    Input.slider
        [ Element.height (Element.px 30)

        -- Here is where we're creating/styling the "track"
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 2)
                , Element.centerY
                , Background.color grey
                , Border.rounded 2
                ]
                Element.none
            )
        ]
        { onChange = round >> onChange
        , label = Input.labelAbove [] (text label)
        , min = toFloat min
        , max = toFloat max
        , step = Just 1
        , value = toFloat (field model)
        , thumb = Input.defaultThumb
        }


grey : Element.Color
grey =
    rgb255 100 100 100
