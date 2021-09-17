module Card exposing
    ( Card(..)
    , Color(..)
    , Figure(..)
    , Suit(..)
    , allCards
    , allKinds
    , allValues
    , eq
    , keepFrom
    , keepUntil
    , toColor
    , toFigure
    , toKindString
    , toKindView
    , toSuit
    , toValueString
    , view
    , viewEmpty
    , viewHidden
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra


type Card
    = Card Suit Figure


type Suit
    = Clubs
    | Diamonds
    | Hearts
    | Spades


type Color
    = Black
    | Red


type Figure
    = Ace
    | King
    | Queen
    | Jack
    | Ten
    | Nine
    | Eight
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two


eq : Card -> Card -> Bool
eq (Card suitA figureA) (Card suitB figureB) =
    suitA == suitB && figureA == figureB


keepFrom : Card -> List Card -> List Card
keepFrom card cards =
    case cards of
        head :: tail ->
            if eq head card then
                cards

            else
                keepFrom card tail

        [] ->
            []


keepUntil : Card -> List Card -> List Card
keepUntil card cards =
    case List.Extra.splitWhen (eq card) cards of
        Just ( toKeep, _ ) ->
            toKeep

        Nothing ->
            []


allCards : List Card
allCards =
    allKinds
        |> List.concatMap
            (\kind ->
                List.map
                    (\value -> Card kind value)
                    allValues
            )


allKinds : List Suit
allKinds =
    [ Clubs
    , Diamonds
    , Hearts
    , Spades
    ]


allValues : List Figure
allValues =
    [ Ace
    , King
    , Queen
    , Jack
    , Ten
    , Nine
    , Eight
    , Seven
    , Six
    , Five
    , Four
    , Three
    , Two
    ]


toColor : Card -> Color
toColor (Card kind _) =
    case kind of
        Clubs ->
            Black

        Diamonds ->
            Red

        Hearts ->
            Red

        Spades ->
            Black


toValueString : Card -> String
toValueString (Card _ value) =
    case value of
        Ace ->
            "A"

        King ->
            "K"

        Queen ->
            "Q"

        Jack ->
            "J"

        Ten ->
            "10"

        Nine ->
            "9"

        Eight ->
            "8"

        Seven ->
            "7"

        Six ->
            "6"

        Five ->
            "5"

        Four ->
            "4"

        Three ->
            "3"

        Two ->
            "2"


toKindString : Card -> String
toKindString (Card kind _) =
    case kind of
        Clubs ->
            "♣"

        Diamonds ->
            "♦"

        Hearts ->
            "♥"

        Spades ->
            "♠"


toSuit : Card -> Suit
toSuit (Card suit _) =
    suit


toFigure : Card -> Figure
toFigure (Card _ figure) =
    figure



-- VIEW --


view : Card -> Html msg
view card =
    div
        [ style "background" "white"
        , style "border" "solid 1px black"
        , style "border-radius" "3px"
        , style "width" "4rem"
        , style "height" "5.5rem"
        , style "padding" "0.1rem"
        , style "margin" "0.2rem"
        , style "font-size" "0.75rem"
        ]
        [ div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "height" "100%"
            ]
            [ div
                [ style "display" "flex"
                , style "justify-content" "start"
                ]
                [ div []
                    [ text (card |> toValueString)
                    , br [] []
                    , card |> toKindView
                    ]
                ]
            , div
                [ style "display" "flex"
                , style "justify-content" "center"
                , style "flex-grow" "1"
                , style "align-items" "center"
                , style "font-size" "1.5rem"
                ]
                [ text (card |> toValueString)
                ]
            , div
                [ style "display" "flex"
                , style "justify-content" "end"
                ]
                [ div
                    [ style "transform" "rotate(180deg)"
                    ]
                    [ text (card |> toValueString)
                    , br [] []
                    , card |> toKindView
                    ]
                ]
            ]
        ]


toKindView : Card -> Html msg
toKindView (Card kind _) =
    case kind of
        Clubs ->
            span [ style "color" "black" ] [ text "♣" ]

        Diamonds ->
            span [ style "color" "red" ] [ text "♦" ]

        Hearts ->
            span [ style "color" "red" ] [ text "♥" ]

        Spades ->
            span [ style "color" "black" ] [ text "♠" ]


viewHidden : Html msg
viewHidden =
    div
        [ style "border" "solid 1px black"
        , style "border-radius" "3px"
        , style "background" "beige"
        , style "width" "4rem"
        , style "height" "5.5rem"
        , style "padding" "0.1rem"
        , style "margin" "0.1rem"
        ]
        []


viewEmpty : Html msg
viewEmpty =
    div
        [ style "border" "solid 1px lightgrey"
        , style "border-radius" "3px"
        , style "width" "4rem"
        , style "height" "5.5rem"
        , style "padding" "0.1rem"
        , style "margin" "0.1rem"
        ]
        []
