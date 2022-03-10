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
    , currentRun : List Position
    , algorithm : Algorithm
    }


type alias Grid =
    List (List Cell)


type alias Position =
    ( Int, Int )


type Algorithm
    = BinaryTree
    | Sidewinder


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


type Coin
    = Heads
    | Tails


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

        initialPosition =
            ( 0, 0 )
    in
    ( { grid = List.repeat sizeY (List.repeat sizeX (Cell True True))
      , sizeX = sizeX
      , sizeY = sizeY
      , cellSize = cellSize
      , currentPosition = initialPosition
      , currentRun = []
      , algorithm = Sidewinder
      }
    , Task.perform TookStep (Task.succeed (Just initialPosition))
    )



-- UPDATE


type Msg
    = TookStep (Maybe Position)
    | RemovedWall Wall
    | FlippedCoin Coin
    | ChangedMazeSize Int
    | ChangedCellSize Int


type Wall
    = Wall Direction Position
    | Random Direction (List Position)
    | NoWall


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TookStep (Just position) ->
            ( { model
                | currentPosition = position
                , currentRun = position :: model.currentRun
              }
            , Random.generate FlippedCoin flipCoin
            )

        TookStep Nothing ->
            -- All done!
            ( model, Cmd.none )

        FlippedCoin coin ->
            update
                (RemovedWall (chooseWallToRemove model coin))
                model

        RemovedWall (Wall direction position) ->
            update
                (TookStep (chooseNextStep model))
                (removeWall model direction position)

        RemovedWall (Random direction positions) ->
            ( model
            , Random.generate RemovedWall (randomWall direction positions)
            )

        RemovedWall NoWall ->
            -- Sometimes we don't want to remove a wall!
            update
                (TookStep (chooseNextStep model))
                model

        ChangedMazeSize newSize ->
            init newSize model.cellSize ()

        ChangedCellSize newSize ->
            init model.sizeX newSize ()


chooseNextStep : Model -> Maybe Position
chooseNextStep model =
    let
        ( x, y ) =
            model.currentPosition
    in
    if (x >= model.sizeX - 1) && (y >= model.sizeY - 1) then
        -- Visited every cell. All done!
        Nothing

    else if x >= model.sizeX - 1 then
        -- Reached the end of this row, drop down to next one
        Just ( 0, y + 1 )

    else
        -- Otherwise take a step to the right
        Just ( x + 1, y )


chooseWallToRemove : Model -> Coin -> Wall
chooseWallToRemove model coin =
    case model.algorithm of
        BinaryTree ->
            chooseWithBinaryTree model coin

        Sidewinder ->
            chooseWithSidewinder model coin


{-| Choose a wall to remove using the BinaryTree maze algorithm
-}
chooseWithBinaryTree : Model -> Coin -> Wall
chooseWithBinaryTree model coin =
    let
        ( x, y ) =
            model.currentPosition
    in
    case coin of
        Heads ->
            if y == 0 && x >= model.sizeX - 1 then
                -- Special case: Don't remove anything from northeast corner
                NoWall

            else if y == 0 then
                -- Special case: Don't remove the northern maze border
                Wall East model.currentPosition

            else
                -- Normal case: Remove northern wall on a Heads
                Wall North model.currentPosition

        Tails ->
            if y == 0 && x >= model.sizeX - 1 then
                -- Special case: Don't remove anything from northeast corner
                NoWall

            else if x >= model.sizeX - 1 then
                -- Special case: Don't remove eastern maze border
                Wall North model.currentPosition

            else
                -- Normal case: Remove eastern wall on a Tails
                Wall East model.currentPosition


{-| Choose a wall to remove using the Sidewinder maze algorithm
-}
chooseWithSidewinder : Model -> Coin -> Wall
chooseWithSidewinder model coin =
    let
        ( x, y ) =
            model.currentPosition
    in
    case coin of
        Heads ->
            if y == 0 && x >= model.sizeX - 1 then
                -- Special case: Don't remove anything from northeast corner
                NoWall

            else if y == 0 then
                -- Special case: Don't remove the northern maze border
                Wall East model.currentPosition

            else
                -- Normal case: Remove northern wall from a randomly selected cell in the current run
                Random North model.currentRun

        Tails ->
            -- Sidewinder == BinaryTree for tails flips
            chooseWithBinaryTree model coin


flipCoin : Random.Generator Coin
flipCoin =
    Random.uniform Heads [ Tails ]


randomWall : Direction -> List Position -> Random.Generator Wall
randomWall direction positions =
    case List.Extra.uncons positions of
        Just ( head, tail ) ->
            Random.uniform head tail
                |> Random.map (Wall direction)

        Nothing ->
            Random.constant NoWall


removeWall : Model -> Direction -> Position -> Model
removeWall model direction position =
    let
        ( x, y ) =
            position

        maybeRow =
            getAt y model.grid

        maybeCell =
            getAt x (Maybe.withDefault [] maybeRow)
    in
    case ( maybeRow, maybeCell ) of
        ( Just row, Just cell ) ->
            let
                ( newCell, newRun ) =
                    case direction of
                        North ->
                            ( { cell | north = False }, [] )

                        East ->
                            ( { cell | east = False }, model.currentRun )

                newRow =
                    row |> setAt x newCell
            in
            { model
                | grid = setAt y newRow model.grid
                , currentPosition = ( x + 1, y )
                , currentRun = newRun
            }

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
        column [ padding 20 ]
            [ row [ paddingXY 0 5 ] [ text "Algorithm: Binary Tree (TODO: algorithm selection!)" ]
            , row [ Font.size 18 ] [ text "Fast but makes sucky mazes (paths skew up-and-right)" ]
            , row [ paddingXY 0 20 ]
                [ column [ padding 20, alignTop ]
                    [ row []
                        [ slider
                            { label = "Maze Size: " ++ String.fromInt model.sizeX
                            , onChange = ChangedMazeSize
                            , model = model
                            , field = .sizeX
                            , range = ( 3, 80 )
                            }
                        ]
                    , row []
                        [ slider
                            { label = "Path Size: " ++ String.fromInt model.cellSize
                            , onChange = ChangedCellSize
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
