module Tableau exposing
    ( place
    , splitAt
    )

import Commons exposing (c2, c3, c4, c5, c6, h4, tableau1)
import Expect
import Klondike.Tableau as Tableau exposing (Tableau(..))
import Test exposing (..)


splitAt : Test
splitAt =
    describe "Tableau.splitAt"
        [ test "should return a stack of cards starting from the requested card" <|
            \_ ->
                Tableau.splitAt c3 tableau1
                    |> Expect.equal
                        ( Tableau.Tableau { cards = [ c2 ], showFrom = 0 }
                        , [ c5, c4, c3 ]
                        )
        , test "should return the original tableau when the requested card isn't found" <|
            \_ ->
                Tableau.splitAt c6 tableau1
                    |> Expect.equal
                        ( tableau1, [] )
        , test "should return the original tableau when placing cards back" <|
            \_ ->
                (Tableau.splitAt c3 tableau1
                    |> (\( newTableau, cards ) ->
                            Tableau.forcePlace newTableau cards
                       )
                )
                    |> Expect.equal
                        tableau1
        ]


place : Test
place =
    describe "Tableau.place"
        [ test "should return Nothing when the move is illegal" <|
            \_ ->
                Tableau.place tableau1 [ c6 ]
                    |> Expect.equal Nothing
        , test "should return the Tableau with its new cards when the move is legal" <|
            \_ ->
                Tableau.place tableau1 [ h4 ]
                    |> Expect.equal (Just (Tableau.forcePlace tableau1 [ h4 ]))
        ]
