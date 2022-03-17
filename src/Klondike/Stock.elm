module Klondike.Stock exposing
    ( Stock
    , empty
    , fromCards
    , getCards
    , head
    , pick
    , place
    )

import Card exposing (Card)


type Stock
    = Stock (List Card)


empty : Stock
empty =
    Stock []


fromCards : List Card -> Stock
fromCards cards =
    Stock cards


pick : Stock -> ( Stock, Maybe Card )
pick ((Stock cards) as stockStack) =
    case cards of
        card :: remaining ->
            ( Stock remaining, Just card )

        [] ->
            ( stockStack, Nothing )


place : Stock -> Card -> Stock
place (Stock cards) card =
    Stock (card :: cards)


getCards : Stock -> List Card
getCards (Stock cards) =
    cards


head : Stock -> Maybe Card
head (Stock cards) =
    case cards of
        card :: _ ->
            Just card

        _ ->
            Nothing
