module Klondike.Foundation exposing
    ( Foundation
    , empty
    , forcePlace
    , fromCards
    , getCards
    , head
    , isCardPlaceable
    , pick
    , place
    )

import Card exposing (Card(..), Figure(..))


type Foundation
    = Foundation (List Card)


empty : Foundation
empty =
    Foundation []


fromCards : List Card -> Foundation
fromCards cards =
    Foundation cards


pick : Foundation -> ( Foundation, Maybe Card )
pick ((Foundation cards) as foundationStack) =
    case cards of
        card :: remaining ->
            ( Foundation remaining, Just card )

        [] ->
            ( foundationStack, Nothing )


head : Foundation -> Maybe Card
head (Foundation cards) =
    List.head cards


{-| Place a Card onto the Foundation without caring for rules
-}
forcePlace : Foundation -> Card -> Foundation
forcePlace (Foundation cards) card =
    Foundation (card :: cards)


{-| Place a List Card onto the Foundation.

Returns Nothing when the move is illegal.

-}
place : Foundation -> Card -> Maybe Foundation
place foundation card =
    if isCardPlaceable foundation card then
        Just (forcePlace foundation card)

    else
        Nothing


{-| You can place aces on an empty Foundation.
-}
isCardPlaceable : Foundation -> Card -> Bool
isCardPlaceable foundation (Card suit figure) =
    case foundation |> head of
        Just fHead ->
            isSuitPlaceable fHead suit && isFigurePlaceable fHead figure

        Nothing ->
            figure == Ace


isFigurePlaceable : Card -> Card.Figure -> Bool
isFigurePlaceable card figure =
    card
        |> placeableFigure
        |> Maybe.map ((==) figure)
        |> Maybe.withDefault False


isSuitPlaceable : Card -> Card.Suit -> Bool
isSuitPlaceable (Card suit _) =
    (==) suit


getCards : Foundation -> List Card
getCards (Foundation cards) =
    cards


placeableFigure : Card -> Maybe Figure
placeableFigure card =
    case Card.toFigure card of
        King ->
            Nothing

        Queen ->
            Just King

        Jack ->
            Just Queen

        Ten ->
            Just Jack

        Nine ->
            Just Ten

        Eight ->
            Just Nine

        Seven ->
            Just Eight

        Six ->
            Just Seven

        Five ->
            Just Six

        Four ->
            Just Five

        Three ->
            Just Four

        Two ->
            Just Three

        Ace ->
            Just Two
