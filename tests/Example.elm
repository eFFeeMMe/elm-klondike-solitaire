module Example exposing (listExtraTests, suite)

import Card
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
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


tableau1 =
    Tableau.Tableau { cards = [ c5, c4, c3, c2 ], showFrom = 0 }


suite : Test
suite =
    describe "Tableau"
        [ test "splitAt should return a stack of cards starting from the requested card" <|
            \_ ->
                Expect.equal
                    (Tableau.splitAt c3 tableau1)
                    ( Tableau.Tableau { cards = [ c2 ], showFrom = 0 }
                    , [ c5, c4, c3 ]
                    )
        , test "splitAt should return the original tableau when the requested card isn't found" <|
            \_ ->
                Expect.equal
                    (Tableau.splitAt c6 tableau1)
                    ( tableau1, [] )
        , test "sassari" <|
            \_ ->
                Expect.equal
                    (Tableau.splitAt c3 tableau1
                        |> (\( tableau2, grabbedCards ) -> Tableau.place tableau2 grabbedCards)
                    )
                    tableau1
        ]


listExtraTests : Test
listExtraTests =
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
