module Klondike.Waste exposing (..)

import Card exposing (Card)


type Waste
    = Waste (List Card)


wastePick : Waste -> ( Waste, Maybe Card )
wastePick ((Waste cards) as wasteStack) =
    case cards of
        card :: remaining ->
            ( Waste remaining, Just card )

        [] ->
            ( wasteStack, Nothing )


wastePlace : Waste -> Card -> Waste
wastePlace (Waste cards) card =
    Waste (card :: cards)


wasteCards : Waste -> List Card
wasteCards (Waste cards) =
    cards


wasteTopCard : Waste -> Maybe Card
wasteTopCard (Waste cards) =
    List.head cards
