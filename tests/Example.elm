module Example exposing
    ( listExtra
    , place
    , splitAt
    )

import Card
import Expect
import Klondike.Tableau as Tableau exposing (Tableau(..))
import List.Extra
import Test exposing (..)


c2 =
    Card.Card Card.Clubs Card.Two


c3 =
    Card.Card Card.Clubs Card.Three


c4 =
    Card.Card Card.Clubs Card.Four


c5 =
    Card.Card Card.Clubs Card.Five


c6 =
    Card.Card Card.Clubs Card.Six


h4 =
    Card.Card Card.Hearts Card.Four


tableau1 =
    Tableau.Tableau { cards = [ c5, c4, c3, c2 ], showFrom = 0 }


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


{-| This is just here as a reminder of how to use List.Extra functions
-}
listExtra : Test
listExtra =
    describe "List.Extra functions"
        [ test "I needed an example to see how List.Extra.splitWhen works :<" <|
            \_ ->
                Expect.equal
                    (List.Extra.splitWhen ((==) 3) [ 2, 3, 4 ])
                    (Just ( [ 2 ], [ 3, 4 ] ))
        , test "I needed an example to see how List.Extra.splitAt works :<" <|
            \_ ->
                Expect.equal
                    (List.Extra.splitAt 2 [ 2, 3, 4 ])
                    ( [ 2, 3 ], [ 4 ] )
        ]
