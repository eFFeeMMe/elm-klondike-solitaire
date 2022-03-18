module Klondike.Waste exposing
    ( Waste
    , empty
    , getCards
    , head
    , pickHead
    , place
    )

import Card exposing (Card)


type Waste
    = Waste (List Card)


empty : Waste
empty =
    Waste []


pickHead : Waste -> ( Waste, Maybe Card )
pickHead ((Waste cards) as wasteStack) =
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
