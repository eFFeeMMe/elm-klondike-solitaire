module Klondike.Foundation exposing
    ( Foundation(..)
    , forcePlace
    , getCards
    , head
    , isCardPlaceable
    , pick
    , place
    )

import Card exposing (Card(..), Figure(..))


type Foundation
    = Foundation (List Card)


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


{-| You can place to an empty Foundation.
-}
isCardPlaceable : Foundation -> Card -> Bool
isCardPlaceable foundation (Card suit figure) =
    case foundation |> head of
        Just fHead ->
            case
                ( fHead |> Card.toSuit |> isSuitPlaceable suit
                , fHead |> Card.toFigure |> isFigurePlaceable figure
                )
            of
                ( True, True ) ->
                    True

                ( _, _ ) ->
                    False

        Nothing ->
            True


isFigurePlaceable : Card.Figure -> Card.Figure -> Bool
isFigurePlaceable below above =
    case placeableFigure below of
        Just requiredFigure ->
            requiredFigure == above

        Nothing ->
            True


isSuitPlaceable : Card.Suit -> Card.Suit -> Bool
isSuitPlaceable =
    (==)


getCards : Foundation -> List Card
getCards (Foundation cards) =
    cards


placeableFigure : Figure -> Maybe Figure
placeableFigure figure =
    case figure of
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
