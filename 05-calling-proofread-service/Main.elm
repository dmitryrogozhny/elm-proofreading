module Main exposing (..)

import Html exposing (Html, button, div, span, text, textarea)
import Html.Attributes exposing (attribute, class, value)
import Html.Events exposing (on, onClick, onInput, onMouseLeave)
import Http
import Json.Decode exposing (Decoder, at, field, int, list, map3, string)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Comment =
    { message : String
    , offset : Int
    , length : Int
    }


type alias Model =
    { text : String
    , comments : List Comment
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model "" [], Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = SetText String
    | Proofread
    | ProofreadResult (Result Http.Error (List Comment))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetText text ->
            ( { model | text = text, comments = [] }, Cmd.none )

        Proofread ->
            ( model, requestProofread model.text )

        ProofreadResult (Ok comments) ->
            ( { model | comments = comments }, Cmd.none )

        ProofreadResult (Err error) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ textarea [ onInput SetText, value model.text, class "text-editor" ] []
        , button [ onClick Proofread ] [ text "Proofread" ]
        , div [ class "proofread-panel" ] [ text model.text ]
        , viewCommentsCount model
        ]


viewCommentsCount : Model -> Html msg
viewCommentsCount model =
    div [ class "note" ] [ text ("comments: " ++ toString (List.length model.comments)) ]



-- HTTP


requestProofread : String -> Cmd Msg
requestProofread text =
    let
        url =
            "https://languagetool.org/api/v2/check"

        body =
            Http.stringBody "application/x-www-form-urlencoded" (encodeProofreadRequest text)

        request =
            Http.post url body commentListDecoder
    in
    Http.send ProofreadResult request


encodeProofreadRequest : String -> String
encodeProofreadRequest text =
    String.join "&"
        [ "text=" ++ Http.encodeUri text
        , "language=" ++ "en-US"
        ]


commentDecoder : Decoder Comment
commentDecoder =
    map3 Comment
        (field "message" string)
        (field "offset" int)
        (field "length" int)


commentListDecoder : Decoder (List Comment)
commentListDecoder =
    at [ "matches" ] (list commentDecoder)
