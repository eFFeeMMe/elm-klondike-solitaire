module Klondike.Waste exposing
    ( Waste
    , empty
    , fromCards
    , getCards
    , head
    , pick
    , place
    )

import Card exposing (Card)


type Waste
    = Waste (List Card)


empty : Waste
empty =
    Waste []


fromCards : List Card -> Waste
fromCards cards =
    Waste cards


pick : Waste -> ( Waste, Maybe Card )
pick ((Waste cards) as wasteStack) =
    case cards of
        card :: remaining ->
            ( Waste remaining, Just card )

        [] ->
            ( wasteStack, Nothing )


place : Waste -> Card -> Waste
place (Waste wasteCards) card =
    Waste (card :: wasteCards)


getCards : Waste -> List Card
getCards (Waste cards) =
    cards


head : Waste -> Maybe Card
head (Waste cards) =
    List.head cards
