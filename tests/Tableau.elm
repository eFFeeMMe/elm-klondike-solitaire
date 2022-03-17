module Tableau exposing
    ( place
    , splitAt
    )

import Commons exposing (c2, c3, c4, c5, c6, h4, tableau1)
import Expect
import Klondike.Tableau as Tableau
import Test


splitAt : Test.Test
splitAt =
    Test.describe "Tableau.splitAt"
        [ Test.test "should return a stack of cards starting from the requested card" <|
            \_ ->
                Tableau.splitAtCard c3 tableau1
                    |> Expect.equal
                        ( Tableau.fromCards [ c2 ]
                        , [ c5, c4, c3 ]
                        )
        , Test.test "should return the original tableau when the requested card isn't found" <|
            \_ ->
                Tableau.splitAtCard c6 tableau1
                    |> Expect.equal
                        ( tableau1, [] )
        , Test.test "should return the original tableau when picking up cards and placing them back" <|
            \_ ->
                (Tableau.splitAtCard c3 tableau1
                    |> (\( newTableau, cards ) ->
                            Tableau.forcePlace newTableau cards
                       )
                )
                    |> Expect.equal
                        tableau1
        ]


place : Test.Test
place =
    Test.describe "Tableau.place"
        [ Test.test "should return Nothing when the move is illegal" <|
            \_ ->
                Tableau.place tableau1 [ c6 ]
                    |> Expect.equal Nothing
        , Test.test "should return the Tableau with its new cards when the move is legal" <|
            \_ ->
                Tableau.place tableau1 [ h4 ]
                    |> Expect.equal (Just (Tableau.forcePlace tableau1 [ h4 ]))
        ]
