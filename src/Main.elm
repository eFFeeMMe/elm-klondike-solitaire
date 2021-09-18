module Main exposing (..)

import Browser
import Browser.Navigation
import Card exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Klondike.Klondike as Klondike
import Random
import Random.List
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { key : Browser.Navigation.Key
    , url : Url.Url
    , klondike : Klondike.Model
    }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , url = url
      , klondike = Klondike.initEmpty
      }
    , allCards |> Random.List.shuffle |> Random.generate SetDeck
    )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SetDeck (List Card)
    | KlondikeMsg Klondike.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        SetDeck deck ->
            ( { model | klondike = Klondike.initFromFullDeck deck }, Cmd.none )

        KlondikeMsg klondikeMsg ->
            ( { model | klondike = Klondike.update model.klondike klondikeMsg }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Solitaire"
    , body =
        [ Klondike.view KlondikeMsg model.klondike
        ]
    }


viewURLStuff : Model -> Html msg
viewURLStuff model =
    div
        []
        [ text "The current URL is: "
        , b [] [ text (Url.toString model.url) ]
        , ul []
            [ viewLink "/home"
            , viewLink "/profile"
            ]
        ]


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
