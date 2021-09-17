module Klondike.Waste exposing (..)

import Card exposing (Card)


type Waste
    = Waste (List Card)


pick : Waste -> ( Waste, Maybe Card )
pick ((Waste cards) as wasteStack) =
    case cards of
        card :: remaining ->
            ( Waste remaining, Just card )

        [] ->
            ( wasteStack, Nothing )


place : Waste -> Card -> Waste
place (Waste cards) card =
    Waste (card :: cards)


getCards : Waste -> List Card
getCards (Waste cards) =
    cards


wasteTopCard : Waste -> Maybe Card
wasteTopCard (Waste cards) =
    List.head cards
