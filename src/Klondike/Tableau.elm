module Klondike.Tableau exposing
    ( Tableau(..)
    , getCards
    , head
    , isCardAppendable
    , pick
    , place
    )

import Card exposing (Card(..), Color(..), Figure(..))


type Tableau
    = Tableau
        { cards : List Card
        , showFrom : Int
        }


pick : Tableau -> ( Tableau, Maybe Card )
pick ((Tableau ({ cards } as tableauStackRecord)) as tableauStack) =
    case cards of
        card :: remaining ->
            ( Tableau { tableauStackRecord | cards = remaining }, Just card )

        [] ->
            ( tableauStack, Nothing )


head : Tableau -> Maybe Card
head (Tableau { cards }) =
    List.head cards


place : Tableau -> List Card -> Tableau
place (Tableau tableauStack) card =
    Tableau { tableauStack | cards = card ++ tableauStack.cards }


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
