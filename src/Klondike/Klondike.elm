module Klondike.Klondike exposing (..)

import Card exposing (Card)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, preventDefaultOn)
import Json.Decode
import Json.Encode
import Klondike.Foundation as Foundation exposing (Foundation)
import Klondike.Stock as Stock exposing (Stock)
import Klondike.Tableau as Tableau exposing (Tableau)
import Klondike.Waste as Waste exposing (Waste)


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
    , mousePosition : MousePosition
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
    | NoOp
    | DragStart
    | DragDrop
    | MouseMove MousePosition


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

        NoOp ->
            model

        DragStart ->
            model

        DragDrop ->
            model

        MouseMove mousePosition ->
            { model | mousePosition = mousePosition }


initEmpty : Model
initEmpty =
    { stock = Stock.empty
    , waste = Waste.empty
    , foundation1 = Foundation.empty
    , foundation2 = Foundation.empty
    , foundation3 = Foundation.empty
    , foundation4 = Foundation.empty
    , tableau1 = Tableau.empty
    , tableau2 = Tableau.empty
    , tableau3 = Tableau.empty
    , tableau4 = Tableau.empty
    , tableau5 = Tableau.empty
    , tableau6 = Tableau.empty
    , tableau7 = Tableau.empty
    , interaction = NotDragging
    , mousePosition = MousePosition 0 0
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
    init { initEmpty | stock = Stock.fromCards cards } initPlacements
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
                        , stock = Stock.fromCards cards
                    }

                [] ->
                    { model
                        | stock = Stock.fromCards (Waste.getCards model.waste)
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
            case Waste.pickHead model.waste of
                ( waste, Just card ) ->
                    { model
                        | interaction = DraggingCardFrom PWaste card
                        , waste = waste
                    }

                ( _, Nothing ) ->
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
            [ onMouseMove msgTagger ]
            [ div
                [ style "display" "flex"
                , style "justify-content" "space-between"
                , style "margin-bottom" "1rem"
                ]
                [ div [ style "display" "flex" ]
                    [ viewStock (msgTagger ClickedStock) model
                    , viewWaste (msgTagger ClickedWaste) model
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
        , viewInteraction msgTagger model.mousePosition model.interaction
        ]


viewStock : msg -> Model -> Html msg
viewStock onClickMsg { stock, interaction } =
    div
        []
        [ case interaction of
            NotDragging ->
                case Stock.head stock of
                    Just card ->
                        Card.view onClickMsg card

                    Nothing ->
                        Card.viewEmpty onClickMsg

            DraggingCardFrom PStock _ ->
                Card.viewHidden onClickMsg

            DraggingCardsFrom PStock _ ->
                Card.viewHidden onClickMsg

            _ ->
                case Stock.head stock of
                    Just card ->
                        Card.view onClickMsg card

                    Nothing ->
                        Card.viewEmpty onClickMsg
        ]


viewWaste : msg -> Model -> Html msg
viewWaste onClickMsg { waste } =
    div
        [ style "margin-right" "1rem"
        ]
        [ waste
            |> Waste.head
            |> Maybe.map (Card.view onClickMsg)
            |> Maybe.withDefault
                (Card.viewEmpty onClickMsg)
        ]


viewFoundation : (Msg -> msg) -> Position -> Foundation -> Html msg
viewFoundation msgTagger position foundation =
    div
        []
        [ foundation
            |> Foundation.head
            |> Maybe.map (Card.view (msgTagger (ClickedFoundation position foundation)))
            |> Maybe.withDefault (Card.viewEmpty (msgTagger (ClickedFoundation position foundation)))
        ]


viewTableau : (Msg -> msg) -> Position -> Tableau -> Html msg
viewTableau msgTagger position tableau =
    let
        cards =
            Tableau.getCards tableau

        hiddenCards =
            Tableau.getHiddenCards tableau
    in
    case ( cards, hiddenCards ) of
        ( [], [] ) ->
            div
                [ onClick (msgTagger (ClickedTableau position tableau Nothing))
                ]
                [ Card.viewEmpty (msgTagger (ClickedTableau position tableau Nothing))
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
                    |> List.indexedMap
                        (\i card ->
                            div
                                [ style "z-index" (String.fromInt i)
                                , style "margin-top" "-4rem"
                                ]
                                [ card ]
                        )
                )


viewTableauCard : (Msg -> msg) -> Position -> Tableau -> Card -> Html msg
viewTableauCard msgTagger position tableau card =
    div
        [ preventDefaultOnDragOver (msgTagger NoOp)
        , onDragStart (msgTagger (ClickedTableau position tableau (Just card)))
        , onDragDrop (msgTagger (ClickedTableau position tableau (Just card)))
        ]
        [ Card.view (msgTagger (ClickedTableau position tableau (Just card))) card
        ]


type alias MousePosition =
    { x : Int
    , y : Int
    }


onMouseMove : (Msg -> msg) -> Attribute msg
onMouseMove msgTagger =
    on
        "mousemove"
        (Json.Decode.map2
            MousePosition
            (Json.Decode.field "x" Json.Decode.int)
            (Json.Decode.field "y" Json.Decode.int)
            |> Json.Decode.map MouseMove
            |> Json.Decode.map msgTagger
        )


viewInteraction : (Msg -> msg) -> MousePosition -> Interaction -> Html msg
viewInteraction msgTagger mousePosition interaction =
    let
        display =
            case interaction of
                NotDragging ->
                    "none"

                _ ->
                    "block"

        left =
            mousePosition.x
                |> (+) 50
                |> String.fromInt
                |> (\str -> String.append str "px")

        top =
            mousePosition.y
                |> (+) 50
                |> String.fromInt
                |> (\str -> String.append str "px")
    in
    div
        [ style "display" display
        , style "position" "absolute"
        , style "z-index" "999"
        , style "left" left
        , style "top" top
        ]
        (case interaction of
            NotDragging ->
                []

            DraggingCardFrom _ card ->
                [ Card.view (msgTagger NoOp) card ]

            DraggingCardsFrom _ cards ->
                cards
                    |> List.map (Card.view (msgTagger NoOp))
                    |> List.indexedMap
                        (\i card ->
                            div
                                [ style "z-index" (String.fromInt i)
                                , style "margin-top" "-4rem"
                                ]
                                [ card ]
                        )
        )


onDragStart : msg -> Attribute msg
onDragStart msg =
    preventDefaultOn "dragstart" (Json.Decode.succeed ( msg, True ))


onDragDrop : msg -> Attribute msg
onDragDrop msg =
    preventDefaultOn "drop" (Json.Decode.succeed ( msg, True ))


preventDefaultOnDragOver : msg -> Attribute msg
preventDefaultOnDragOver msg =
    on "dragover" (Json.Decode.succeed msg)



-- hijackOn : String -> D.Decoder msg -> Attribute msg
-- hijackOn event decoder =
--     preventDefaultOn event (D.map hijack decoder)
-- hijack : msg -> ( msg, Bool )
-- hijack msg =
--     ( msg, True )


viewHiddenTableauCard : (Msg -> msg) -> Position -> Tableau -> Html msg
viewHiddenTableauCard msgTagger position tableau =
    div
        [ onClick (msgTagger (ClickedTableau position tableau Nothing))
        ]
        [ Card.viewHidden (msgTagger (ClickedTableau position tableau Nothing))
        ]
