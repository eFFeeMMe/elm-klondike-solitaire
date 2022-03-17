module Klondike.Klondike exposing (..)

import Card
    exposing
        ( Card(..)
        , Color(..)
        , Figure(..)
        , viewHidden
        )
import Css exposing (position)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Klondike.Foundation as Foundation exposing (Foundation(..))
import Klondike.Stock as Stock exposing (Stock(..))
import Klondike.Tableau as Tableau exposing (Tableau(..))
import Klondike.Waste as Waste


type alias Model =
    { stock : Stock
    , waste : Waste.Waste
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
    | ClickedTableau Position Tableau (Maybe Card)
    | ClickedFoundation Position Foundation


update : Model -> Msg -> Model
update model msg =
    case msg of
        ClickedStock ->
            clickStock model

        ClickedWaste ->
            clickWaste model

        ClickedFoundation position foundation ->
            clickFoundation position foundation model

        ClickedTableau position tableau card ->
            clickTableau position tableau card model


initEmpty : Model
initEmpty =
    { stock = Stock []
    , waste = Waste.empty
    , foundation1 = Foundation []
    , foundation2 = Foundation []
    , foundation3 = Foundation []
    , foundation4 = Foundation []
    , tableau1 = Tableau { cards = [], hiddenCards = [] }
    , tableau2 = Tableau { cards = [], hiddenCards = [] }
    , tableau3 = Tableau { cards = [], hiddenCards = [] }
    , tableau4 = Tableau { cards = [], hiddenCards = [] }
    , tableau5 = Tableau { cards = [], hiddenCards = [] }
    , tableau6 = Tableau { cards = [], hiddenCards = [] }
    , tableau7 = Tableau { cards = [], hiddenCards = [] }
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
    init { initEmpty | stock = Stock cards } initPlacements
        |> hideTableauCards


initPlacements : List Position
initPlacements =
    List.concat
        [ List.repeat 1 PTableau1
        , List.repeat 2 PTableau2
        , List.repeat 3 PTableau3
        , List.repeat 4 PTableau4
        , List.repeat 5 PTableau5
        , List.repeat 6 PTableau6
        , List.repeat 7 PTableau7
        ]


initPlaceCard : Model -> Position -> Model
initPlaceCard model position =
    let
        ( stock, card ) =
            Stock.pick model.stock
    in
    case card of
        Just card_ ->
            forcePlaceCard { model | stock = stock } position card_

        Nothing ->
            model


hideTableauCards : Model -> Model
hideTableauCards model =
    { model
        | tableau1 = model.tableau1 |> Tableau.hideAllCards |> Tableau.uncover
        , tableau2 = model.tableau2 |> Tableau.hideAllCards |> Tableau.uncover
        , tableau3 = model.tableau3 |> Tableau.hideAllCards |> Tableau.uncover
        , tableau4 = model.tableau4 |> Tableau.hideAllCards |> Tableau.uncover
        , tableau5 = model.tableau5 |> Tableau.hideAllCards |> Tableau.uncover
        , tableau6 = model.tableau6 |> Tableau.hideAllCards |> Tableau.uncover
        , tableau7 = model.tableau7 |> Tableau.hideAllCards |> Tableau.uncover
    }


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


setFoundationByPosition : Position -> Foundation -> Model -> Model
setFoundationByPosition position foundation model =
    case position of
        PFoundation1 ->
            { model | foundation1 = foundation }

        PFoundation2 ->
            { model | foundation2 = foundation }

        PFoundation3 ->
            { model | foundation3 = foundation }

        PFoundation4 ->
            { model | foundation4 = foundation }

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


forcePlaceCard : Model -> Position -> Card -> Model
forcePlaceCard model position card =
    case position of
        PStock ->
            { model | stock = Stock.place model.stock card }

        PWaste ->
            { model | waste = Waste.place model.waste card }

        PFoundation1 ->
            { model | foundation1 = Foundation.forcePlace model.foundation1 card }

        PFoundation2 ->
            { model | foundation2 = Foundation.forcePlace model.foundation2 card }

        PFoundation3 ->
            { model | foundation3 = Foundation.forcePlace model.foundation3 card }

        PFoundation4 ->
            { model | foundation4 = Foundation.forcePlace model.foundation4 card }

        PTableau1 ->
            { model | tableau1 = Tableau.forcePlace model.tableau1 [ card ] }

        PTableau2 ->
            { model | tableau2 = Tableau.forcePlace model.tableau2 [ card ] }

        PTableau3 ->
            { model | tableau3 = Tableau.forcePlace model.tableau3 [ card ] }

        PTableau4 ->
            { model | tableau4 = Tableau.forcePlace model.tableau4 [ card ] }

        PTableau5 ->
            { model | tableau5 = Tableau.forcePlace model.tableau5 [ card ] }

        PTableau6 ->
            { model | tableau6 = Tableau.forcePlace model.tableau6 [ card ] }

        PTableau7 ->
            { model | tableau7 = Tableau.forcePlace model.tableau7 [ card ] }


forcePlaceCards : Model -> Position -> List Card -> Model
forcePlaceCards model position cards =
    case cards of
        card :: tailCards ->
            model
                |> (\m -> forcePlaceCard m position card)
                |> (\m -> forcePlaceCards m position tailCards)

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
                |> (\m -> forcePlaceCard m position card)

        DraggingCardsFrom position cards ->
            model
                |> (\m -> { m | interaction = NotDragging })
                |> (\m -> forcePlaceCards m position (List.reverse cards))


uncoverTableaus : Model -> Model
uncoverTableaus model =
    { model
        | tableau1 = Tableau.uncover model.tableau1
        , tableau2 = Tableau.uncover model.tableau2
        , tableau3 = Tableau.uncover model.tableau3
        , tableau4 = Tableau.uncover model.tableau4
        , tableau5 = Tableau.uncover model.tableau5
        , tableau6 = Tableau.uncover model.tableau6
        , tableau7 = Tableau.uncover model.tableau7
    }


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
                    { model
                        | stock = Stock (Waste.getCards model.waste)
                        , waste = Waste.empty
                    }

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
            case Waste.getCards model.waste of
                card :: cards ->
                    { model
                        | interaction = DraggingCardFrom PWaste card
                        , waste = Waste.fromCards cards
                    }

                [] ->
                    model

        DraggingCardFrom _ card ->
            { model
                | interaction = NotDragging
                , waste = Waste.place model.waste card
            }

        DraggingCardsFrom _ _ ->
            model


clickTableau : Position -> Tableau -> Maybe Card -> Model -> Model
clickTableau position tableau card model =
    case model.interaction of
        NotDragging ->
            case card of
                Nothing ->
                    model

                Just card_ ->
                    let
                        ( newTableau, grabbedCards ) =
                            Tableau.splitAtCard card_ tableau
                    in
                    model
                        |> setInteraction (DraggingCardsFrom position grabbedCards)
                        |> setTableauByPosition position newTableau

        DraggingCardFrom _ card_ ->
            case Tableau.place tableau [ card_ ] of
                Just tableau_ ->
                    model
                        |> setInteraction NotDragging
                        |> setTableauByPosition position tableau_
                        |> uncoverTableaus

                Nothing ->
                    undoDragging model

        DraggingCardsFrom _ cards ->
            case Tableau.place tableau cards of
                Just tableau_ ->
                    model
                        |> setInteraction NotDragging
                        |> setTableauByPosition position tableau_
                        |> uncoverTableaus

                Nothing ->
                    undoDragging model


clickFoundation : Position -> Foundation -> Model -> Model
clickFoundation position foundation model =
    case model.interaction of
        NotDragging ->
            let
                ( newFoundation, grabbedCard ) =
                    Foundation.pick foundation
            in
            case grabbedCard of
                Just grabbedCard_ ->
                    model
                        |> setInteraction (DraggingCardFrom position grabbedCard_)
                        |> setFoundationByPosition position newFoundation

                Nothing ->
                    model

        DraggingCardFrom _ card_ ->
            case Foundation.place foundation card_ of
                Just tableau_ ->
                    model
                        |> setInteraction NotDragging
                        |> setFoundationByPosition position tableau_
                        |> uncoverTableaus

                Nothing ->
                    undoDragging model

        DraggingCardsFrom _ cards ->
            case cards of
                [ card ] ->
                    case Foundation.place foundation card of
                        Just tableau_ ->
                            model
                                |> setInteraction NotDragging
                                |> setFoundationByPosition position tableau_
                                |> uncoverTableaus

                        Nothing ->
                            undoDragging model

                _ ->
                    undoDragging model



-- VIEW


view : (Msg -> msg) -> Model -> Html msg
view msgTagger model =
    div
        [ style "margin" "1rem"
        ]
        [ div
            []
            [ div
                [ style "display" "flex"
                , style "justify-content" "space-between"
                , style "margin-bottom" "1rem"
                ]
                [ div [ style "display" "flex" ]
                    [ viewStock msgTagger model
                    , viewWaste msgTagger model
                    ]
                , viewFoundation msgTagger PFoundation1 model.foundation1
                , viewFoundation msgTagger PFoundation2 model.foundation2
                , viewFoundation msgTagger PFoundation3 model.foundation3
                , viewFoundation msgTagger PFoundation4 model.foundation4
                ]
            , div
                [ style "display" "flex"
                , style "justify-content" "space-between"
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


viewStock : (Msg -> msg) -> Model -> Html msg
viewStock msgTagger { stock, interaction } =
    div
        [ onClick (msgTagger ClickedStock)
        ]
        [ case interaction of
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
        ]


viewWaste : (Msg -> msg) -> Model -> Html msg
viewWaste msgTagger { waste } =
    div
        [ onClick (msgTagger ClickedWaste)
        , style "margin-right" "1rem"
        ]
        [ waste
            |> Waste.head
            |> Maybe.map Card.view
            |> Maybe.withDefault
                Card.viewEmpty
        ]


viewFoundation : (Msg -> msg) -> Position -> Foundation -> Html msg
viewFoundation msgTagger position foundation =
    div
        [ onClick (msgTagger (ClickedFoundation position foundation))
        ]
        [ foundation
            |> Foundation.head
            |> Maybe.map Card.view
            |> Maybe.withDefault Card.viewEmpty
        ]


viewTableau : (Msg -> msg) -> Position -> Tableau -> Html msg
viewTableau msgTagger position ((Tableau { cards, hiddenCards }) as tableau) =
    case ( cards, hiddenCards ) of
        ( [], [] ) ->
            div
                [ onClick (msgTagger (ClickedTableau position tableau Nothing))
                ]
                [ Card.viewEmpty
                ]

        _ ->
            div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "margin-top" "4rem"
                ]
                (List.concat
                    [ hiddenCards
                        |> List.map (always (viewHiddenTableauCard msgTagger position tableau))
                    , cards
                        |> List.reverse
                        |> List.map (viewTableauCard msgTagger position tableau)
                    ]
                    |> List.map List.singleton
                    |> List.indexedMap
                        (\i card ->
                            div
                                [ style "z-index" (String.fromInt i)
                                , style "margin-top" "-4rem"
                                ]
                                card
                        )
                )


viewTableauCard : (Msg -> msg) -> Position -> Tableau -> Card -> Html msg
viewTableauCard msgTagger position tableau card =
    div
        [ onClick (msgTagger (ClickedTableau position tableau (Just card)))
        ]
        [ Card.view card
        ]


viewHiddenTableauCard : (Msg -> msg) -> Position -> Tableau -> Html msg
viewHiddenTableauCard msgTagger position tableau =
    div
        [ onClick (msgTagger (ClickedTableau position tableau Nothing))
        ]
        [ viewHidden
        ]
