module Klondike.Klondike exposing (..)

import Card
    exposing
        ( Card(..)
        , Color(..)
        , Figure(..)
        )
import Css exposing (position)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Klondike.Foundation as Foundation exposing (Foundation(..))
import Klondike.Stock as Stock exposing (Stock(..))
import Klondike.Tableau as Tableau exposing (Tableau(..))
import Klondike.Waste
    exposing
        ( Waste(..)
        , wasteCards
        , wastePick
        , wastePlace
        , wasteTopCard
        )


type alias Model =
    { stock : Stock
    , waste : Waste
    , foundation1 : Foundation
    , foundation2 : Foundation
    , foundation3 : Foundation
    , foundation4 : Foundation
    , tableau1 : Tableau
    , tableau2 : Tableau
    , tableau3 : Tableau
    , tableau4 : Tableau
    , tableau5 : Tableau
    , tableau6 : Tableau
    , tableau7 : Tableau
    , interaction : Interaction
    }


type Interaction
    = NotDragging
    | DraggingCardFrom Position Card
    | DraggingCardsFrom Position (List Card)


type Position
    = PStock
    | PWaste
    | PFoundation1
    | PFoundation2
    | PFoundation3
    | PFoundation4
    | PTableau1
    | PTableau2
    | PTableau3
    | PTableau4
    | PTableau5
    | PTableau6
    | PTableau7


type Msg
    = ClickedStock
    | ClickedWaste
    | ClickedTableau Position Tableau Card


update : Model -> Msg -> Model
update model msg =
    case msg of
        ClickedStock ->
            clickStock model

        ClickedWaste ->
            clickWaste model

        ClickedTableau position tableau card ->
            clickTableau position tableau card model


initEmpty : Model
initEmpty =
    { stock = Stock []
    , waste = Waste []
    , foundation1 = Foundation []
    , foundation2 = Foundation []
    , foundation3 = Foundation []
    , foundation4 = Foundation []
    , tableau1 = Tableau { cards = [], showFrom = 0 }
    , tableau2 = Tableau { cards = [], showFrom = 1 }
    , tableau3 = Tableau { cards = [], showFrom = 2 }
    , tableau4 = Tableau { cards = [], showFrom = 3 }
    , tableau5 = Tableau { cards = [], showFrom = 4 }
    , tableau6 = Tableau { cards = [], showFrom = 5 }
    , tableau7 = Tableau { cards = [], showFrom = 6 }
    , interaction = NotDragging
    }


init : Model -> List Position -> Model
init model placements =
    case placements of
        position :: remainingPlacements ->
            init (initPlaceCard model position) remainingPlacements

        [] ->
            model


initFromFullDeck : List Card -> Model
initFromFullDeck cards =
    let
        model =
            initEmpty
    in
    init { model | stock = Stock cards } initPlacements


initPlacements : List Position
initPlacements =
    List.concat
        [ List.map (always PTableau1) (List.range 1 1)
        , List.map (always PTableau2) (List.range 1 2)
        , List.map (always PTableau3) (List.range 1 3)
        , List.map (always PTableau4) (List.range 1 4)
        , List.map (always PTableau5) (List.range 1 5)
        , List.map (always PTableau6) (List.range 1 6)
        , List.map (always PTableau7) (List.range 1 7)
        ]


initPlaceCard : Model -> Position -> Model
initPlaceCard model position =
    let
        ( stock, card ) =
            Stock.pick model.stock
    in
    case card of
        Just card_ ->
            placeCard { model | stock = stock } position card_

        Nothing ->
            model


setInteraction : Interaction -> Model -> Model
setInteraction interaction model =
    { model | interaction = interaction }


setTableauByPosition : Position -> Tableau -> Model -> Model
setTableauByPosition position tableau model =
    case position of
        PTableau1 ->
            { model | tableau1 = tableau }

        PTableau2 ->
            { model | tableau2 = tableau }

        PTableau3 ->
            { model | tableau3 = tableau }

        PTableau4 ->
            { model | tableau4 = tableau }

        PTableau5 ->
            { model | tableau5 = tableau }

        PTableau6 ->
            { model | tableau6 = tableau }

        PTableau7 ->
            { model | tableau7 = tableau }

        _ ->
            model


getTableauByPosition : Position -> Model -> Maybe Tableau
getTableauByPosition position model =
    case position of
        PTableau1 ->
            Just model.tableau1

        PTableau2 ->
            Just model.tableau2

        PTableau3 ->
            Just model.tableau3

        PTableau4 ->
            Just model.tableau4

        PTableau5 ->
            Just model.tableau5

        PTableau6 ->
            Just model.tableau6

        PTableau7 ->
            Just model.tableau7

        _ ->
            Nothing


placeCard : Model -> Position -> Card -> Model
placeCard model position card =
    case position of
        PStock ->
            model

        PWaste ->
            { model | waste = wastePlace model.waste card }

        PFoundation1 ->
            { model | foundation1 = Foundation.place model.foundation1 card }

        PFoundation2 ->
            { model | foundation2 = Foundation.place model.foundation2 card }

        PFoundation3 ->
            { model | foundation3 = Foundation.place model.foundation3 card }

        PFoundation4 ->
            { model | foundation4 = Foundation.place model.foundation4 card }

        PTableau1 ->
            { model | tableau1 = Tableau.place model.tableau1 [ card ] }

        PTableau2 ->
            { model | tableau2 = Tableau.place model.tableau2 [ card ] }

        PTableau3 ->
            { model | tableau3 = Tableau.place model.tableau3 [ card ] }

        PTableau4 ->
            { model | tableau4 = Tableau.place model.tableau4 [ card ] }

        PTableau5 ->
            { model | tableau5 = Tableau.place model.tableau5 [ card ] }

        PTableau6 ->
            { model | tableau6 = Tableau.place model.tableau6 [ card ] }

        PTableau7 ->
            { model | tableau7 = Tableau.place model.tableau7 [ card ] }


placeCards : Model -> Position -> List Card -> Model
placeCards model position cards =
    case cards of
        card :: tailCards ->
            model
                |> (\m -> placeCard m position card)
                |> (\m -> placeCards m position tailCards)

        [] ->
            model


undoDragging : Model -> Model
undoDragging model =
    case model.interaction of
        NotDragging ->
            model

        DraggingCardFrom position card ->
            model
                |> (\m -> { m | interaction = NotDragging })
                |> (\m -> placeCard m position card)

        DraggingCardsFrom position cards ->
            model
                |> (\m -> { m | interaction = NotDragging })
                |> (\m -> placeCards m position cards)


clickStock : Model -> Model
clickStock model =
    case model.interaction of
        NotDragging ->
            case Stock.getCards model.stock of
                card :: cards ->
                    { model
                        | interaction = DraggingCardFrom PStock card
                        , stock = Stock cards
                    }

                [] ->
                    model

        DraggingCardFrom fromPosition card ->
            case fromPosition of
                PStock ->
                    { model
                        | interaction = NotDragging
                        , stock = Stock.place model.stock card
                    }

                _ ->
                    undoDragging model

        DraggingCardsFrom _ _ ->
            undoDragging model


clickWaste : Model -> Model
clickWaste model =
    case model.interaction of
        NotDragging ->
            case wasteCards model.waste of
                card :: cards ->
                    { model
                        | interaction = DraggingCardFrom PWaste card
                        , waste = Waste cards
                    }

                [] ->
                    model

        DraggingCardFrom _ card ->
            { model
                | interaction = NotDragging
                , waste = wastePlace model.waste card
            }

        DraggingCardsFrom _ _ ->
            model


clickTableau : Position -> Tableau -> Card -> Model -> Model
clickTableau position tableau card model =
    let
        cardsInTableau =
            Tableau.getCards tableau
    in
    case model.interaction of
        NotDragging ->
            let
                ( newTableau, grabbedCards ) =
                    Tableau.splitAt card tableau
            in
            model
                |> setInteraction (DraggingCardsFrom position grabbedCards)
                |> setTableauByPosition position newTableau

        DraggingCardFrom _ card_ ->
            if Tableau.isCardAppendable tableau card_ then
                model
                    |> setInteraction NotDragging
                    |> setTableauByPosition position (Tableau { cards = card_ :: cardsInTableau, showFrom = 0 })

            else
                undoDragging model

        DraggingCardsFrom fromWhere cards ->
            model


view : (Msg -> msg) -> Model -> Html msg
view msgTagger model =
    div
        []
        [ span
            []
            [ text ("Stock contains: " ++ (model.stock |> Stock.getCards |> List.length |> String.fromInt) ++ " items") ]
        , span
            []
            [ text
                ("# of cards: "
                    ++ (((model.stock |> Stock.getCards |> List.length)
                            + (model.waste |> wasteCards |> List.length)
                        )
                            |> String.fromInt
                       )
                    ++ " items"
                )
            ]
        , div
            []
            [ div
                [ style "display" "flex"
                , style "margin-bottom" "50px"
                ]
                [ div
                    [ onClick (msgTagger ClickedStock) ]
                    [ viewStock model ]
                , div
                    [ onClick (msgTagger ClickedWaste) ]
                    [ viewWaste model ]
                , viewFoundation model.foundation1
                , viewFoundation model.foundation2
                , viewFoundation model.foundation3
                , viewFoundation model.foundation4
                ]
            , div
                [ style "display" "flex"
                ]
                [ viewTableau msgTagger PTableau1 model.tableau1
                , viewTableau msgTagger PTableau2 model.tableau2
                , viewTableau msgTagger PTableau3 model.tableau3
                , viewTableau msgTagger PTableau4 model.tableau4
                , viewTableau msgTagger PTableau5 model.tableau5
                , viewTableau msgTagger PTableau6 model.tableau6
                , viewTableau msgTagger PTableau7 model.tableau7
                ]
            ]
        ]


viewStock : Model -> Html msg
viewStock { stock, interaction } =
    case interaction of
        NotDragging ->
            case Stock.head stock of
                Just card ->
                    Card.view card

                Nothing ->
                    Card.viewEmpty

        DraggingCardFrom PStock _ ->
            Card.viewHidden

        DraggingCardsFrom PStock _ ->
            Card.viewHidden

        _ ->
            case Stock.head stock of
                Just card ->
                    Card.view card

                Nothing ->
                    Card.viewEmpty


viewWaste : Model -> Html msg
viewWaste { waste } =
    case wasteTopCard waste of
        Just card ->
            Card.view card

        Nothing ->
            Card.viewEmpty


viewFoundation : Foundation -> Html msg
viewFoundation (Foundation cards) =
    case List.head cards of
        Just card ->
            Card.view card

        Nothing ->
            Card.viewEmpty


viewTableau : (Msg -> msg) -> Position -> Tableau -> Html msg
viewTableau msgTagger position ((Tableau { cards }) as tableau) =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "margin-top" "2rem"
        ]
        (cards
            |> List.reverse
            |> List.indexedMap (\i card -> viewTableauCard msgTagger position tableau card i)
        )


viewTableauCard : (Msg -> msg) -> Position -> Tableau -> Card -> Int -> Html msg
viewTableauCard msgTagger position tableau card i =
    div
        [ onClick (msgTagger (ClickedTableau position tableau card))
        , style "margin-top" "-4rem"
        , style "z-index" (String.fromInt i)
        ]
        [ Card.view card
        ]



-- TODO: https://stackoverflow.com/questions/40311601/elm-live-unable-to-use-custom-html-to-reference-css-file
-- TODO: fix keepFrom/Until usages
