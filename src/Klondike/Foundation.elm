module Klondike.Foundation exposing (..)

import Card exposing (Card)


type Foundation
    = Foundation (List Card)


pick : Foundation -> ( Foundation, Maybe Card )
pick ((Foundation cards) as foundationStack) =
    case cards of
        card :: remaining ->
            ( Foundation remaining, Just card )

        [] ->
            ( foundationStack, Nothing )


place : Foundation -> Card -> Foundation
place (Foundation cards) card =
    Foundation (card :: cards)


getCards : Foundation -> List Card
getCards (Foundation cards) =
    cards
