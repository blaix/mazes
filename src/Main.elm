module Main exposing (main)

import Browser
import Dict exposing (update)
import Element exposing (Element, centerX, centerY, column, el, height, layout, px, row, text, width)
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


type alias Model =
    List (List Cell)


type alias Cell =
    { north : Bool
    , east : Bool
    }


type Direction
    = North
    | East


sizeX : Int
sizeX =
    25


sizeY : Int
sizeY =
    15


init : flags -> ( Model, Cmd Msg )
init _ =
    ( List.repeat sizeY (List.repeat sizeX (Cell True True))
    , carvePathCmd ( 0, 0 )
    )



-- UPDATE


type Msg
    = CarvePath ( Int, Int )
    | RemoveWall ( Int, Int ) Direction


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
            ( model
            , if (y == 0) && (x == sizeX - 1) then
                -- Don't remove any wall in the northeast corner
                if sizeY > 1 then
                    carvePathCmd ( 0, 1 )

                else
                    -- If the maze is one row high (dumb), we're done
                    Cmd.none

              else if y == 0 then
                -- Don't remove the northern maze boundary
                removeWallCmd ( x, y ) East

              else if x == sizeX - 1 then
                -- Don't remove the eastern maze boundary
                removeWallCmd ( x, y ) North

              else
                Random.generate (RemoveWall ( x, y )) randomDirection
            )

        RemoveWall ( x, y ) direction ->
            ( model |> removeWall ( x, y ) direction
            , if x < (sizeX - 1) then
                carvePathCmd ( x + 1, y )

              else if y < (sizeY - 1) then
                carvePathCmd ( 0, y + 1 )

              else
                Cmd.none
            )


removeWall : ( Int, Int ) -> Direction -> Model -> Model
removeWall ( x, y ) direction model =
    let
        maybeRow =
            getAt y model

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
            model |> setAt y newRow

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
            (List.map drawRow model)


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
