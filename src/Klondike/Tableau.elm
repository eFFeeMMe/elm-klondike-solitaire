module Klondike.Tableau exposing
    ( Tableau(..)
    , getCards
    , head
    , isCardAppendable
    , pickHead
    , place
    , splitAt
    )

import Card exposing (Card(..), Color(..), Figure(..))
import List.Extra



--@TODO: drop the "showFrom" crap and actually have a list of hidden cards


type Tableau
    = Tableau
        { cards : List Card
        , showFrom : Int
        }


pickHead : Tableau -> ( Tableau, Maybe Card )
pickHead ((Tableau ({ cards } as tableauStackRecord)) as tableauStack) =
    case cards of
        card :: remaining ->
            ( Tableau { tableauStackRecord | cards = remaining }, Just card )

        [] ->
            ( tableauStack, Nothing )


{-| Picks a stack of cards from the Tableau, starting from the requested card

Example
pickFromCard (C2) Tableau {cards = [C1, C2, C3]}
returns:
(Tableau {cards = [C3]}, [C1, C2])

-}
splitAt : Card -> Tableau -> ( Tableau, List Card )
splitAt from (Tableau { cards, showFrom }) =
    let
        splitIndex =
            cards
                |> List.Extra.elemIndex from
                |> Maybe.withDefault (List.length cards)
                |> (+) 1
    in
    cards
        |> List.Extra.splitAt splitIndex
        |> (\( left, right ) ->
                ( Tableau { cards = right, showFrom = showFrom }
                , left
                )
           )


head : Tableau -> Maybe Card
head (Tableau { cards }) =
    List.head cards


place : Tableau -> List Card -> Tableau
place (Tableau tableauStack) cards =
    Tableau { tableauStack | cards = cards ++ tableauStack.cards }


getCards : Tableau -> List Card
getCards (Tableau { cards }) =
    cards


appendableFigure : Card -> Maybe Figure
appendableFigure (Card _ figure) =
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


appendableColor : Card -> Color
appendableColor card =
    case Card.toColor card of
        Card.Black ->
            Card.Red

        Card.Red ->
            Card.Black


tableauCardAppendabilityRequirements : Card -> Maybe ( Figure, Color )
tableauCardAppendabilityRequirements card =
    appendableFigure card
        |> Maybe.map (\figure -> ( figure, appendableColor card ))


tableauCardAppendabilityRule : Card -> Card -> Bool
tableauCardAppendabilityRule to ((Card _ figure) as card) =
    case tableauCardAppendabilityRequirements to of
        Just ( requiredFigure, requiredColor ) ->
            Card.toColor card == requiredColor && figure == requiredFigure

        Nothing ->
            False


isCardAppendable : Tableau -> Card -> Bool
isCardAppendable tableau card =
    case head tableau of
        Just tableauHead ->
            tableauCardAppendabilityRule tableauHead card

        Nothing ->
            False
