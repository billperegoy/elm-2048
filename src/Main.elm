module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    ()



-- Model


type Element
    = Empty
    | Full Int


type Line
    = Line Element Element Element Element


type Model
    = Board Line Line Line Line


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        row_1 =
            Line Empty Empty Empty Empty

        row_2 =
            Line (Full 2) Empty Empty Empty

        row_3 =
            Line Empty (Full 2) (Full 2) Empty

        row_4 =
            Line (Full 4) (Full 4) (Full 2) (Full 2)
    in
        ( Board row_1 row_2 row_3 row_4, Cmd.none )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    h1 [] [ text "Hello" ]


lineToList : Line -> List Element
lineToList (Line elem_1 elem_2 elem_3 elem_4) =
    [ elem_1, elem_2, elem_3, elem_4 ]


listToLine : List Element -> Line
listToLine elems =
    case elems of
        [ elem_1, elem_2, elem_3, elem_4 ] ->
            Line elem_1 elem_2 elem_3 elem_4

        _ ->
            Debug.todo "Received list with other than 4 elements"


boardToList : Model -> List Line
boardToList (Board line_1 line_2 line_3 line_4) =
    [ line_1, line_2, line_3, line_4 ]


transformLine : Line -> Line
transformLine line =
    shiftLineRight line


shiftLineRight : Line -> Line
shiftLineRight element =
    case element of
        -- One active element
        Line elem Empty Empty Empty ->
            Line Empty Empty Empty elem

        Line Empty elem Empty Empty ->
            Line Empty Empty Empty elem

        Line Empty Empty elem Empty ->
            Line Empty Empty Empty elem

        Line Empty Empty Empty elem ->
            Line Empty Empty Empty elem

        -- Two active elements
        Line elem1 elem2 Empty Empty ->
            Line Empty Empty elem1 elem2

        Line elem1 Empty elem2 Empty ->
            Line Empty Empty elem1 elem2

        Line elem1 Empty Empty elem2 ->
            Line Empty Empty elem1 elem2

        Line Empty elem1 elem2 Empty ->
            Line Empty Empty elem1 elem2

        Line Empty elem1 Empty elem2 ->
            Line Empty Empty elem1 elem2

        Line Empty Empty elem1 elem2 ->
            Line Empty Empty elem1 elem2

        -- Three active elements
        Line elem1 elem2 elem3 Empty ->
            Line Empty elem1 elem2 elem3

        Line elem1 elem2 Empty elem3 ->
            Line Empty elem1 elem2 elem3

        Line elem1 Empty elem2 elem3 ->
            Line Empty elem1 elem2 elem3

        Line Empty elem1 elem2 elem3 ->
            Line Empty elem1 elem2 elem3

        -- Four active elements
        Line elem1 elem2 elem3 elem4 ->
            Line elem1 elem2 elem3 elem4


combineLineRight : Line -> Line
combineLineRight element =
    case element of
        Line Empty Empty (Full c) (Full d) ->
            if c == d then
                Line Empty Empty Empty (Full (c + d))
            else
                element

        Line Empty (Full b) (Full c) (Full d) ->
            if (b == c) && (c /= d) then
                Line Empty Empty (Full (b + c)) (Full d)
            else if (b /= c) && (c == d) then
                Line Empty Empty (Full b) (Full (c + d))
            else
                element

        Line (Full a) (Full b) (Full c) (Full d) ->
            if a == b && c == d then
                Line Empty Empty (Full (a + b)) (Full (c + d))
            else if a /= b && b == c && c /= d then
                Line Empty (Full a) (Full (b + c)) (Full d)
            else if a == b && b /= c then
                Line Empty (Full (a + b)) (Full c) (Full d)
            else if b /= c && c == d then
                Line Empty (Full a) (Full b) (Full (c + d))
            else
                element

        _ ->
            element



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
