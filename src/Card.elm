module Card exposing
    ( Card(..)
    , Color(..)
    , Figure(..)
    , Suit(..)
    , allSuits
    , allValues
    , eq
    , fullDeck
    , keepFrom
    , keepUntil
    , toColor
    , toFigure
    , toSuit
    , toSuitString
    , toSuitView
    , toValueString
    , view
    , viewEmpty
    , viewHidden
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
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


fullDeck : List Card
fullDeck =
    allSuits
        |> List.concatMap
            (\suit ->
                List.map
                    (\value -> Card suit value)
                    allValues
            )


allSuits : List Suit
allSuits =
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
toColor (Card suit _) =
    case suit of
        Clubs ->
            Black

        Diamonds ->
            Red

        Hearts ->
            Red

        Spades ->
            Black


toColorString : Card -> String
toColorString card =
    case toColor card of
        Black ->
            "black"

        Red ->
            "red"


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


toSuitString : Card -> String
toSuitString (Card suit _) =
    case suit of
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


view : msg -> Card -> Html msg
view onClickMsg card =
    div
        (commonCardAttributes
            ++ [ style "background" "white"
               , style "font-size" "0.75rem"
               , style "user-select" "none"
               , onClick onClickMsg
               ]
        )
        [ div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "height" "100%"
            ]
            [ div
                [ style "display" "flex"
                , style "justify-content" "start"
                ]
                [ div
                    [ style "position" "absolute"
                    ]
                    [ div []
                        [ text (card |> toValueString)
                        ]
                    , div []
                        [ card |> toSuitView
                        ]
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
                    , style "position" "absolute"
                    , style "bottom" "0"
                    ]
                    [ div []
                        [ text (card |> toValueString)
                        ]
                    , div []
                        [ card |> toSuitView
                        ]
                    ]
                ]
            ]
        ]


toSuitView : Card -> Html msg
toSuitView card =
    span [ style "color" (toColorString card) ] [ text (toSuitString card) ]


viewHidden : msg -> Html msg
viewHidden onClickMsg =
    div
        (commonCardAttributes
            ++ [ style "background" "beige"
               , onClick onClickMsg
               ]
        )
        []


viewEmpty : msg -> Html msg
viewEmpty onClickMsg =
    div
        (commonCardAttributes
            ++ [ style "border" "none"
               , style "box-shadow" "inset 1px 1px rgba(0, 0, 0, 0.02)"
               , style "background" "rgba(5,3,0,0.03)"
               , onClick onClickMsg
               ]
        )
        []


commonCardAttributes : List (Html.Attribute msg)
commonCardAttributes =
    [ style "border" "solid 1px rgba(5,3,0,0.1)"
    , style "box-shadow" "0px 1px 2px 0px rgba(0, 0, 0, 0.2)"
    , style "border-radius" "3px"
    , style "width" "4rem"
    , style "height" "5.5rem"
    , style "padding" "0.1rem"
    , style "position" "relative"
    , style "margin" "0.1rem"
    ]
