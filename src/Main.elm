module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Element
    exposing
        ( Element
        , alignRight
        , alignTop
        , centerY
        , column
        , el
        , fill
        , height
        , image
        , layout
        , link
        , padding
        , paddingXY
        , px
        , rgb255
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as Json
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key exposing (Key(..))
import List exposing (range)
import List.Extra exposing (getAt, setAt)
import Random
import Task
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init defaults
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Config =
    { size : Int
    , cellSize : Int
    , algorithm : Algorithm
    , carveDelay : Int
    }


defaults : Config
defaults =
    { size = 40
    , cellSize = 25
    , algorithm = Sidewinder
    , carveDelay = 0
    }


{-| Get a config based on current model.
Useful when resetting the board.
-}
config : Model -> Config
config model =
    { size = model.sizeX
    , cellSize = model.cellSize
    , algorithm = model.algorithm
    , carveDelay = model.carveDelay
    }



-- MODEL


type alias Model =
    { grid : Grid
    , sizeX : Int
    , sizeY : Int
    , cellSize : Int
    , carvingPosition : Position
    , carvingRun : List Position
    , solvingPosition : Position
    , solvingRun : List Position
    , algorithm : Algorithm
    , carved : Bool
    , carveDelay : Int
    }


{-| A 2-dimensional grid of cells
-}
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
    , position : Position
    }


type Direction
    = North
    | East


type Coin
    = Heads
    | Tails


type alias Walls =
    { north : Bool
    , east : Bool
    , south : Bool
    , west : Bool
    }


{-| Get the walls for a given cell position
-}
getWalls : Model -> Position -> Walls
getWalls model position =
    let
        ( x, y ) =
            position

        cell : Maybe Cell
        cell =
            getAt y model.grid
                |> Maybe.andThen (getAt x)

        southernNeighbor : Maybe Cell
        southernNeighbor =
            getAt (y - 1) model.grid
                |> Maybe.andThen (getAt x)

        westernNeighbor : Maybe Cell
        westernNeighbor =
            getAt y model.grid
                |> Maybe.andThen (getAt (x - 1))

        ( north, east ) =
            case cell of
                Just c ->
                    ( c.north, c.east )

                Nothing ->
                    ( True, True )

        south =
            case southernNeighbor of
                Just c ->
                    c.north

                Nothing ->
                    True

        west =
            case westernNeighbor of
                Just c ->
                    c.east

                Nothing ->
                    True
    in
    { north = north
    , east = east
    , south = south
    , west = west
    }


initialPosition : Position
initialPosition =
    ( 0, 0 )


wallWidth : Int
wallWidth =
    2


{-| Create the maze and start carving in the top left cell.
-}
init : Config -> flags -> ( Model, Cmd Msg )
init { size, cellSize, algorithm, carveDelay } _ =
    let
        sizeX =
            size

        sizeY =
            round (toFloat size * (4 / 6))

        initCell position =
            Cell True True position
    in
    ( { grid =
            List.Extra.initialize sizeY
                (\y -> List.Extra.initialize sizeX (\x -> initCell ( x, y )))
      , sizeX = sizeX
      , sizeY = sizeY
      , cellSize = cellSize
      , carvingPosition = initialPosition
      , carvingRun = []
      , solvingPosition = ( 0, sizeY - 1 )
      , solvingRun = []
      , algorithm = algorithm
      , carved = False
      , carveDelay = carveDelay
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
    | ChangedAlgorithm Algorithm
    | ChangedCarveDelay Int
    | KeyPressed KeyboardEvent
    | Waiting


type Wall
    = Wall Direction Position
    | Random Direction (List Position)
    | NoWall


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TookStep (Just position) ->
            ( { model
                | carvingPosition = position
                , carvingRun = position :: model.carvingRun
              }
            , Random.generate FlippedCoin flipCoin
            )

        TookStep Nothing ->
            -- All done!
            ( { model | carved = True }, Cmd.none )

        FlippedCoin coin ->
            update
                (RemovedWall (chooseWallToRemove model coin))
                model

        RemovedWall (Wall direction position) ->
            update
                (takeStepOrWait model)
                (removeWall model direction position)

        RemovedWall (Random direction positions) ->
            ( model
            , Random.generate RemovedWall (randomWall direction positions)
            )

        RemovedWall NoWall ->
            -- Sometimes we don't want to remove a wall!
            update
                (takeStepOrWait model)
                model

        ChangedMazeSize newSize ->
            let
                cfg =
                    config model
            in
            init { cfg | size = newSize } ()

        ChangedCellSize newSize ->
            let
                cfg =
                    config model
            in
            init { cfg | cellSize = newSize } ()

        ChangedAlgorithm algorithm ->
            let
                cfg =
                    config model
            in
            init { cfg | algorithm = algorithm } ()

        ChangedCarveDelay delay ->
            let
                cfg =
                    config model
            in
            init { cfg | carveDelay = delay } ()

        KeyPressed event ->
            let
                ( x, y ) =
                    model.solvingPosition

                walls =
                    getWalls model model.solvingPosition

                solvingPosition =
                    if event.keyCode == Up && y > 0 && not walls.north then
                        ( x, y - 1 )

                    else if event.keyCode == Down && y < model.sizeY - 1 && not walls.south then
                        ( x, y + 1 )

                    else if event.keyCode == Right && x < model.sizeX - 1 && not walls.east then
                        ( x + 1, y )

                    else if event.keyCode == Left && x > 0 && not walls.west then
                        ( x - 1, y )

                    else
                        ( x, y )
            in
            ( { model | solvingPosition = solvingPosition }, Cmd.none )

        Waiting ->
            ( model, Cmd.none )


takeStepOrWait : Model -> Msg
takeStepOrWait model =
    if model.carveDelay == 0 then
        TookStep (chooseNextStep model)

    else
        -- If we're not instantly carving,
        -- wait for timer to trigger next step.
        Waiting


chooseNextStep : Model -> Maybe Position
chooseNextStep model =
    let
        ( x, y ) =
            model.carvingPosition
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
<http://weblog.jamisbuck.org/2011/2/1/maze-generation-binary-tree-algorithm>
-}
chooseWithBinaryTree : Model -> Coin -> Wall
chooseWithBinaryTree model coin =
    let
        ( x, y ) =
            model.carvingPosition
    in
    case coin of
        Heads ->
            if y == 0 && x >= model.sizeX - 1 then
                -- Special case: Don't remove anything from northeast corner
                NoWall

            else if y == 0 then
                -- Special case: Don't remove the northern maze border
                Wall East model.carvingPosition

            else
                -- Normal case: Remove northern wall on a Heads
                Wall North model.carvingPosition

        Tails ->
            if y == 0 && x >= model.sizeX - 1 then
                -- Special case: Don't remove anything from northeast corner
                NoWall

            else if x >= model.sizeX - 1 then
                -- Special case: Don't remove eastern maze border
                Wall North model.carvingPosition

            else
                -- Normal case: Remove eastern wall on a Tails
                Wall East model.carvingPosition


{-| Choose a wall to remove using the Sidewinder maze algorithm
<https://weblog.jamisbuck.org/2011/2/3/maze-generation-sidewinder-algorithm>
-}
chooseWithSidewinder : Model -> Coin -> Wall
chooseWithSidewinder model coin =
    let
        ( x, y ) =
            model.carvingPosition
    in
    case coin of
        Heads ->
            if y == 0 && x >= model.sizeX - 1 then
                -- Special case: Don't remove anything from northeast corner
                NoWall

            else if y == 0 then
                -- Special case: Don't remove the northern maze border
                Wall East model.carvingPosition

            else
                -- Normal case: Remove northern wall from a randomly selected cell in the current run
                Random North
                    -- Exclude cells along the northern maze boundary
                    (List.filter (\( _, cy ) -> cy > 0) model.carvingRun)

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
                            ( { cell | east = False }, model.carvingRun )

                newRow =
                    row |> setAt x newCell
            in
            { model
                | grid = setAt y newRow model.grid
                , carvingRun = newRun
            }

        -- If we got a point that's out of bounds there's nothing we can do
        _ ->
            model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        keySub =
            onKeyDown (Json.map KeyPressed decodeKeyboardEvent)
    in
    if model.carved || model.carveDelay == 0 then
        -- No timer sub because we're done carving
        keySub

    else
        Sub.batch
            [ Time.every
                (toFloat model.carveDelay)
                (always (TookStep (chooseNextStep model)))
            , keySub
            ]



-- VIEW


view : Model -> Html Msg
view model =
    layout [ width fill ] <|
        column [ padding 20, width fill ]
            [ row [ paddingXY 0 5, width fill ]
                [ Input.radioRow [ paddingXY 10 0, spacing 20 ]
                    { onChange = ChangedAlgorithm
                    , selected = Just model.algorithm
                    , label = Input.labelLeft [] (text "Algorithm: ")
                    , options =
                        [ Input.option BinaryTree (text "BinaryTree")
                        , Input.option Sidewinder (text "Sidewinder")
                        ]
                    }
                , el [ alignRight ]
                    (link []
                        { url = "https://github.com/blaix/mazes"
                        , label =
                            image []
                                { src = "GitHub-Mark-32px.png"
                                , description = "Github logo - link to project"
                                }
                        }
                    )
                ]
            , row [ paddingXY 0 20 ]
                [ column [ padding 20, width (px 200), alignTop ]
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
                            , range = ( 4, 100 )
                            }
                        ]
                    , row []
                        [ slider
                            { label = "Delay (ms): " ++ String.fromInt model.carveDelay
                            , onChange = ChangedCarveDelay
                            , model = model
                            , field = .carveDelay
                            , range = ( 0, 200 )
                            }
                        ]
                    ]
                , column []
                    [ column
                        [ Border.widthEach
                            { top = 0
                            , right = 0
                            , bottom = wallWidth
                            , left = wallWidth
                            }
                        ]
                        (List.map (drawRow model) model.grid)
                    , row [ paddingXY 0 6, Font.size 18 ]
                        [ text "Use arrow keys to move" ]
                    ]
                ]
            ]


drawRow : Model -> List Cell -> Element Msg
drawRow model cells =
    row [] (List.map (drawCell model) cells)


drawCell : Model -> Cell -> Element Msg
drawCell model cell =
    let
        borderTop =
            if cell.north then
                wallWidth

            else
                0

        borderRight =
            if cell.east then
                wallWidth

            else
                0

        contents =
            if cell.position == model.solvingPosition then
                "⛄"

            else
                " "
    in
    -- TODO: switch to tailwind? should be setting border width for EVERY cell and just changing the top/right color.
    -- Doesn't seem possible with elm-ui?
    el
        [ width (px model.cellSize)
        , height (px model.cellSize)
        , padding 0
        , spacing 0
        , Font.size model.cellSize
        , Border.color black
        , Border.widthEach
            { top = borderTop
            , right = borderRight
            , bottom = 0
            , left = 0
            }
        ]
        (text contents)


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
                [ width (px 120)
                , height (px 3)
                , centerY
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
    rgb255 110 110 110


black : Element.Color
black =
    rgb255 0 0 0
