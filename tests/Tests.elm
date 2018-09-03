module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Main exposing (..)


all : Test
all =
    describe "A Test Suite"
        [ test "shiftLineRight, 0 elements" <|
            \() ->
                let
                    input =
                        Line Empty Empty Empty Empty

                    expected =
                        Line Empty Empty Empty Empty
                in
                    Expect.equal (shiftLineRight input) expected
        , test "shiftLineRight, 1 element, pos 1" <|
            \() ->
                let
                    input =
                        Line (Full 1) Empty Empty Empty

                    expected =
                        Line Empty Empty Empty (Full 1)
                in
                    Expect.equal (shiftLineRight input) expected
        , test "shiftLineRight, 1 element, pos 2" <|
            \() ->
                let
                    input =
                        Line Empty (Full 1) Empty Empty

                    expected =
                        Line Empty Empty Empty (Full 1)
                in
                    Expect.equal (shiftLineRight input) expected
        , test "shiftLineRight, 1 element, pos 3" <|
            \() ->
                let
                    input =
                        Line Empty Empty (Full 1) Empty

                    expected =
                        Line Empty Empty Empty (Full 1)
                in
                    Expect.equal (shiftLineRight input) expected
        , test "shiftLineRight, 1 element, pos 4" <|
            \() ->
                let
                    input =
                        Line Empty Empty Empty (Full 1)

                    expected =
                        Line Empty Empty Empty (Full 1)
                in
                    Expect.equal (shiftLineRight input) expected
        , test "shiftLineRight, 2 elements, pos 1,2" <|
            \() ->
                let
                    input =
                        Line (Full 1) (Full 2) Empty Empty

                    expected =
                        Line Empty Empty (Full 1) (Full 2)
                in
                    Expect.equal (shiftLineRight input) expected
        , test "shiftLineRight, 2 elements, pos 1,3" <|
            \() ->
                let
                    input =
                        Line (Full 1) Empty (Full 2) Empty

                    expected =
                        Line Empty Empty (Full 1) (Full 2)
                in
                    Expect.equal (shiftLineRight input) expected
        , test "shiftLineRight, 2 elements, pos 1,4" <|
            \() ->
                let
                    input =
                        Line (Full 1) Empty Empty (Full 2)

                    expected =
                        Line Empty Empty (Full 1) (Full 2)
                in
                    Expect.equal (shiftLineRight input) expected
        , test "shiftLineRight, 2 elements, pos 2,3" <|
            \() ->
                let
                    input =
                        Line Empty (Full 1) (Full 2) Empty

                    expected =
                        Line Empty Empty (Full 1) (Full 2)
                in
                    Expect.equal (shiftLineRight input) expected
        , test "shiftLineRight, 2 elements, pos 2,4" <|
            \() ->
                let
                    input =
                        Line Empty (Full 1) Empty (Full 2)

                    expected =
                        Line Empty Empty (Full 1) (Full 2)
                in
                    Expect.equal (shiftLineRight input) expected
        , test "shiftLineRight, 2 elements, pos 3,4" <|
            \() ->
                let
                    input =
                        Line Empty Empty (Full 1) (Full 2)

                    expected =
                        Line Empty Empty (Full 1) (Full 2)
                in
                    Expect.equal (shiftLineRight input) expected
        , test "shiftLineRight, 3 elements, pos 1,2,3" <|
            \() ->
                let
                    input =
                        Line (Full 1) (Full 2) (Full 4) Empty

                    expected =
                        Line Empty (Full 1) (Full 2) (Full 4)
                in
                    Expect.equal (shiftLineRight input) expected
        , test "shiftLineRight, 3 elements, pos 1,3,4" <|
            \() ->
                let
                    input =
                        Line (Full 1) Empty (Full 2) (Full 4)

                    expected =
                        Line Empty (Full 1) (Full 2) (Full 4)
                in
                    Expect.equal (shiftLineRight input) expected
        , test "shiftLineRight, 3 elements, pos 1,2,4" <|
            \() ->
                let
                    input =
                        Line (Full 1) (Full 2) Empty (Full 4)

                    expected =
                        Line Empty (Full 1) (Full 2) (Full 4)
                in
                    Expect.equal (shiftLineRight input) expected
        , test "shiftLineRight, 3 elements, pos 2,3,4" <|
            \() ->
                let
                    input =
                        Line Empty (Full 1) (Full 2) (Full 4)

                    expected =
                        Line Empty (Full 1) (Full 2) (Full 4)
                in
                    Expect.equal (shiftLineRight input) expected
        , test "shiftLineRight, 4 elements" <|
            \() ->
                let
                    input =
                        Line (Full 1) (Full 2) (Full 4) (Full 8)

                    expected =
                        Line (Full 1) (Full 2) (Full 4) (Full 8)
                in
                    Expect.equal (shiftLineRight input) expected
        , test "combineLineRight 1" <|
            \() ->
                let
                    input =
                        Line Empty Empty (Full 1) (Full 1)

                    expected =
                        Line Empty Empty Empty (Full 2)
                in
                    Expect.equal (combineLineRight input) expected
        , test "combineLineRight 2" <|
            \() ->
                let
                    input =
                        Line Empty (Full 1) (Full 1) (Full 4)

                    expected =
                        Line Empty Empty (Full 2) (Full 4)
                in
                    Expect.equal (combineLineRight input) expected
        , test "combineLineRight 3" <|
            \() ->
                let
                    input =
                        Line Empty (Full 1) (Full 2) (Full 2)

                    expected =
                        Line Empty Empty (Full 1) (Full 4)
                in
                    Expect.equal (combineLineRight input) expected
        , test "combineLineRight 4" <|
            \() ->
                let
                    input =
                        Line (Full 1) (Full 1) (Full 4) (Full 4)

                    expected =
                        Line Empty Empty (Full 2) (Full 8)
                in
                    Expect.equal (combineLineRight input) expected
        , test "combineLineRight 5" <|
            \() ->
                let
                    input =
                        Line (Full 1) (Full 2) (Full 2) (Full 8)

                    expected =
                        Line Empty (Full 1) (Full 4) (Full 8)
                in
                    Expect.equal (combineLineRight input) expected
        , test "combineLineRight 6" <|
            \() ->
                let
                    input =
                        Line (Full 1) (Full 1) (Full 4) (Full 8)

                    expected =
                        Line Empty (Full 2) (Full 4) (Full 8)
                in
                    Expect.equal (combineLineRight input) expected
        , test "combineLineRight 7" <|
            \() ->
                let
                    input =
                        Line (Full 1) (Full 2) (Full 4) (Full 4)

                    expected =
                        Line Empty (Full 1) (Full 2) (Full 8)
                in
                    Expect.equal (combineLineRight input) expected
        ]
