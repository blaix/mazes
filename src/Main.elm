module Main exposing (main)

import Browser
import Dict exposing (update)
import Element exposing (Element, centerX, centerY, column, el, height, layout, px, row, text, width)
import Element.Border as Border
import Html exposing (Html)
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


sizeX : Int
sizeX =
    5


sizeY : Int
sizeY =
    5


init : flags -> ( Model, Cmd Msg )
init _ =
    ( List.repeat sizeY (List.repeat sizeX (Cell True True))
    , Task.perform CarvePath (Task.succeed ( 0, 0 ))
    )



-- UPDATE


type Msg
    = CarvePath ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CarvePath ( _, _ ) ->
            -- TODO: carve a path!
            ( model, Cmd.none )



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
