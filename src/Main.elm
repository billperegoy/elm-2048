module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


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


type alias Model =
    { board : Board }


type Element
    = Empty
    | Full Int


type Board
    = Board Line Line Line Line


type Line
    = Line Element Element Element Element


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        line1 =
            Line (Full 4) (Full 4) (Full 2) (Full 2)

        line2 =
            Line (Full 8) (Full 16) (Full 32) (Full 32)

        line3 =
            Line (Full 256) (Full 128) (Full 128) (Full 64)

        line4 =
            Line (Full 512) (Full 512) (Full 1024) (Full 2048)
    in
        ( { board = Board line1 line2 line3 line4 }, Cmd.none )



-- Update


type Msg
    = CombineLeft
    | CombineRight


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CombineRight ->
            let
                lines =
                    boardToList model.board

                newLines =
                    List.map transformLineRight lines

                newBoard =
                    listToBoard newLines
            in
                ( { model | board = newBoard }, Cmd.none )

        CombineLeft ->
            let
                lines =
                    boardToList model.board

                newLines =
                    List.map transformLineLeft lines

                newBoard =
                    listToBoard newLines
            in
                ( { model | board = newBoard }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    let
        lines =
            boardToList model.board
    in
        div
            [ style "margin-left" "100px"
            , style "margin-top" "100px"
            ]
            [ button
                [ style "width" "100px"
                , style "height" "50px"
                , style "font-size" "20px"
                , onClick CombineLeft
                ]
                [ text "Left" ]
            , button
                [ style "width" "100px"
                , style "height" "50px"
                , style "font-size" "20px"
                , onClick CombineRight
                ]
                [ text "Right" ]
            , table
                [ style "margin-top" "20px"
                , style "border" "1px solid black"
                ]
                (List.map
                    viewRow
                    lines
                )
            ]


viewRow line =
    tr []
        (List.map
            (\element -> viewElement element)
            (line |> lineToList)
        )


viewElement : Element -> Html Msg
viewElement element =
    let
        value =
            case element of
                Empty ->
                    ""

                Full n ->
                    String.fromInt n

        cellColor =
            case element of
                Empty ->
                    "white"

                Full n ->
                    color n
    in
        td
            [ style "width" "150px"
            , style "height" "150px"
            , style "font-size" "36pt"
            , style "border" "1px solid black"
            , style "background-color" cellColor
            , align "center"
            ]
            [ (text
                value
              )
            ]


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


transformLineRight : Line -> Line
transformLineRight line =
    line
        |> shiftLineRight
        |> combineLineRight


transformLineLeft : Line -> Line
transformLineLeft line =
    line
        |> shiftLineLeft
        |> combineLineLeft


shiftLineLeft : Line -> Line
shiftLineLeft line =
    line
        |> lineReverse
        |> shiftLineRight
        |> lineReverse


lineReverse : Line -> Line
lineReverse (Line elem1 elem2 elem3 elem4) =
    Line elem4 elem3 elem2 elem1


combineLineLeft : Line -> Line
combineLineLeft line =
    line
        |> lineReverse
        |> combineLineRight
        |> lineReverse


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


color : Int -> String
color num =
    case num of
        2 ->
            "#F1EA7F"

        4 ->
            "#BE9EC9"

        8 ->
            "#D5AE41"

        16 ->
            "#EAE6DA"

        32 ->
            "#D1B894"

        64 ->
            "#ECDB54"

        128 ->
            "#DBB1CD"

        256 ->
            "#EC9787"

        512 ->
            "#BFD641"

        1024 ->
            "#C48F65"

        2048 ->
            "#9C9A40"

        _ ->
            "white"


boardToList : Board -> List Line
boardToList (Board line1 line2 line3 line4) =
    [ line1, line2, line3, line4 ]


listToBoard : List Line -> Board
listToBoard lines =
    case lines of
        [ line1, line2, line3, line4 ] ->
            Board line1 line2 line3 line4

        _ ->
            Debug.todo "Bad board list"



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
