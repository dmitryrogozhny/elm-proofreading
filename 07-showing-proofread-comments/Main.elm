module Main exposing (..)

import Html exposing (Attribute, Html, button, div, span, text, textarea)
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


sampleText : String
sampleText =
    "What do you thinks of grammar checkers? Please not that they are not perfect. Style issues get a blue marker: It's 5 P.M. in the afternoon. LanguageTool 3.8 was released on Thursday, 27 June 2017."


type alias Comment =
    { message : String
    , offset : Int
    , length : Int
    }


type alias Model =
    { text : String
    , comments : List Comment
    , activeCommentId : Maybe Int
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model "" [] Nothing, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = SetText String
    | Proofread
    | ProofreadResult (Result Http.Error (List Comment))
    | AddSampleText
    | SetActiveComment String
    | ResetActiveComment


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
            ( { model | text = toString error }, Cmd.none )

        AddSampleText ->
            ( { model | text = sampleText, comments = [] }, Cmd.none )

        SetActiveComment activeCommentIdStr ->
            let
                result =
                    String.toInt activeCommentIdStr
            in
            case result of
                Ok activeCommentId ->
                    ( { model | activeCommentId = Just activeCommentId }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ResetActiveComment ->
            ( { model | activeCommentId = Nothing }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ textarea [ onInput SetText, value model.text, class "text-editor" ] []
        , button [ onClick AddSampleText ] [ text "Add Sample Text" ]
        , button [ onClick Proofread ] [ text "Proofread" ]
        , viewCommentsPanel model.text model.comments
        , viewCommentsCount model
        , viewActiveCommentPanel model.activeCommentId model.comments
        ]


viewCommentsCount : Model -> Html msg
viewCommentsCount model =
    div [ class "note" ] [ text ("comments: " ++ toString (List.length model.comments)) ]


viewCommentsPanel : String -> List Comment -> Html Msg
viewCommentsPanel reviewedText comments =
    div [ class "proofread-panel" ]
        (viewHighlightedText reviewedText 0 comments)


viewHighlightedText : String -> Int -> List Comment -> List (Html Msg)
viewHighlightedText reviewedText lastProcessedIndex comments =
    case comments of
        [] ->
            let
                restOfText =
                    String.slice lastProcessedIndex (String.length reviewedText) reviewedText
            in
            span [ class "text" ] [ text restOfText ] :: []

        comment :: xComments ->
            let
                leftText =
                    String.slice lastProcessedIndex comment.offset reviewedText

                commentEndIndex =
                    comment.offset + comment.length

                commentText =
                    String.slice comment.offset commentEndIndex reviewedText
            in
            span [ class "text" ] [ text leftText ]
                :: span
                    [ class "comment"
                    , attribute "id" (toString comment.offset)
                    , handleOnMouseEnter SetActiveComment
                    , handleOnMouseLeave ResetActiveComment
                    , handleOnClick SetActiveComment
                    ]
                    [ text commentText ]
                :: viewHighlightedText reviewedText commentEndIndex xComments


viewActiveCommentPanel : Maybe Int -> List Comment -> Html msg
viewActiveCommentPanel activeComment comments =
    case activeComment of
        Nothing ->
            div [] []

        Just activeCommentId ->
            let
                comment =
                    List.head <| List.filter (\comment -> comment.offset == activeCommentId) comments
            in
            case comment of
                Nothing ->
                    div [] []

                Just comment ->
                    div [ class "comment-description" ] [ text comment.message ]



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



-- HELPERS


handleOnMouseLeave : msg -> Attribute msg
handleOnMouseLeave msg =
    onMouseLeave msg


handleOnMouseEnter : (String -> msg) -> Attribute msg
handleOnMouseEnter tagger =
    on "mouseenter" (Json.Decode.map tagger targetDataId)


handleOnClick : (String -> msg) -> Attribute msg
handleOnClick tagger =
    on "click" (Json.Decode.map tagger targetDataId)


targetDataId : Decoder String
targetDataId =
    at [ "target", "id" ] string
