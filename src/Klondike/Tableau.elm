module Klondike.Tableau exposing
    ( Tableau(..)
    , areCardsPlaceable
    , forcePlace
    , getCards
    , head
    , hideAllCards
    , isCardPlaceable
    , pickHead
    , place
    , splitAt
    , uncover
    )

import Card exposing (Card(..), Color(..), Figure(..))
import List.Extra


type Tableau
    = Tableau
        { cards : List Card
        , hiddenCards : List Card
        }


pickHead : Tableau -> ( Tableau, Maybe Card )
pickHead ((Tableau ({ cards } as tableauRecord)) as tableau) =
    case cards of
        card :: remaining ->
            ( Tableau { tableauRecord | cards = remaining }, Just card )

        [] ->
            ( tableau, Nothing )


pickHiddenHead : Tableau -> ( Tableau, Maybe Card )
pickHiddenHead ((Tableau ({ hiddenCards } as tableauRecord)) as tableau) =
    case hiddenCards of
        card :: remaining ->
            ( Tableau { tableauRecord | hiddenCards = remaining }, Just card )

        [] ->
            ( tableau, Nothing )


{-| Picks a stack of cards from the Tableau, starting from the requested card

Example
pickFromCard (C2) Tableau {cards = [C1, C2, C3]}
returns:
(Tableau {cards = [C3]}, [C1, C2])

-}
splitAt : Card -> Tableau -> ( Tableau, List Card )
splitAt from (Tableau tableauRecord) =
    let
        splitIndex =
            tableauRecord.cards
                |> List.Extra.elemIndex from
                |> Maybe.withDefault -1
                |> (+) 1
    in
    tableauRecord.cards
        |> List.Extra.splitAt splitIndex
        |> (\( left, right ) ->
                ( Tableau { tableauRecord | cards = right }
                , left
                )
           )


head : Tableau -> Maybe Card
head (Tableau { cards }) =
    List.head cards


{-| Place a List Card onto the Tableau without caring for the rules
-}
forcePlace : Tableau -> List Card -> Tableau
forcePlace (Tableau tableauRecord) cards =
    Tableau { tableauRecord | cards = cards ++ tableauRecord.cards }


{-| Place a List Card onto the Tableau.

Returns Nothing when the move is illegal.

-}
place : Tableau -> List Card -> Maybe Tableau
place tableau cards =
    if areCardsPlaceable tableau cards then
        Just (forcePlace tableau cards)

    else
        Nothing


getCards : Tableau -> List Card
getCards (Tableau { cards }) =
    cards


placeableFigure : Card -> Maybe Figure
placeableFigure (Card _ figure) =
    case figure of
        King ->
            Just Queen

        Queen ->
            Just Jack

        Jack ->
            Just Ten

        Ten ->
            Just Nine

        Nine ->
            Just Eight

        Eight ->
            Just Seven

        Seven ->
            Just Six

        Six ->
            Just Five

        Five ->
            Just Four

        Four ->
            Just Three

        Three ->
            Just Two

        Two ->
            Just Ace

        Ace ->
            Nothing


placeableColor : Card -> Color
placeableColor card =
    case Card.toColor card of
        Card.Black ->
            Card.Red

        Card.Red ->
            Card.Black


tableauCardPlaceabilityRequirements : Card -> Maybe ( Figure, Color )
tableauCardPlaceabilityRequirements card =
    placeableFigure card
        |> Maybe.map (\figure -> ( figure, placeableColor card ))


tableauCardPlaceabilityRule : Card -> Card -> Bool
tableauCardPlaceabilityRule to ((Card _ figure) as card) =
    case tableauCardPlaceabilityRequirements to of
        Just ( requiredFigure, requiredColor ) ->
            Card.toColor card == requiredColor && figure == requiredFigure

        Nothing ->
            False


{-| You can place Kings onto an empty Tableau.
-}
isCardPlaceable : Tableau -> Card -> Bool
isCardPlaceable tableau ((Card _ figure) as card) =
    tableau
        |> head
        |> Maybe.map (\tableauHead -> tableauCardPlaceabilityRule tableauHead card)
        |> Maybe.withDefault (figure == King)


areCardsPlaceable : Tableau -> List Card -> Bool
areCardsPlaceable tableau cards =
    cards
        |> List.Extra.last
        |> Maybe.map (isCardPlaceable tableau)
        |> Maybe.withDefault False


hideAllCards : Tableau -> Tableau
hideAllCards (Tableau { cards, hiddenCards }) =
    Tableau { cards = [], hiddenCards = cards ++ hiddenCards }


{-| Shows the topmost (head) card of the Tableau if it's hidden
-}
uncover : Tableau -> Tableau
uncover tableau =
    case head tableau of
        Nothing ->
            case pickHiddenHead tableau of
                ( Tableau tableauRecord, Just card ) ->
                    Tableau { cards = [ card ], hiddenCards = tableauRecord.hiddenCards }

                ( tableau_, Nothing ) ->
                    tableau_

        _ ->
            tableau
