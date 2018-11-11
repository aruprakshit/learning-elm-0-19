module Main exposing (Model, Msg(..), getRandomGif, gifDecoder, init, main, subscriptions, toGiphyUrl, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Url.Builder as Url



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Giphy =
    { url : String
    , title : String
    }


type alias Model =
    { topic : String
    , giphy : Giphy
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "cat" (Giphy "waiting.gif" "")
    , getRandomGif "cat"
    )



-- UPDATE


type Msg
    = MorePlease
    | NewGif (Result Http.Error Giphy)
    | ChangeTopic String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model
            , getRandomGif model.topic
            )

        ChangeTopic topicName ->
            ( { model | topic = topicName }
            , Cmd.none
            )

        NewGif result ->
            case result of
                Ok { url, title } ->
                    let
                        oldGiphy =
                            model.giphy

                        newGiphy =
                            { oldGiphy | url = url, title = title }
                    in
                    ( { model | giphy = newGiphy }
                    , Cmd.none
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.topic ]
        , input
            [ type_ "text"
            , onInput ChangeTopic
            ]
            []
        , button [ onClick MorePlease ] [ text "More Please!" ]
        , br [] []
        , img [ src model.giphy.url ] []
        , br [] []
        , strong [] [ text model.giphy.title ]
        ]



-- HTTP


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    Http.send NewGif (Http.get (toGiphyUrl topic) gifDecoder)


toGiphyUrl : String -> String
toGiphyUrl topic =
    Url.crossOrigin "https://api.giphy.com"
        [ "v1", "gifs", "random" ]
        [ Url.string "api_key" "dc6zaTOxFJmzC"
        , Url.string "tag" topic
        ]


gifDecoder : Decode.Decoder Giphy
gifDecoder =
    Decode.map2 Giphy (Decode.field "data" (Decode.field "image_url" Decode.string)) (Decode.field "data" (Decode.field "title" Decode.string))
