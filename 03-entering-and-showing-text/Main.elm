import Html exposing (div, text, textarea)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)

main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL
type alias Model =
    { text : String
    }

model : Model
model =
    Model ""


-- UPDATE
type Msg
    = SetText String

update : Msg -> Model -> Model
update msg model =
    case msg of
        SetText text ->
            { model | text = text }


-- VIEW
view : Model -> Html.Html Msg
view model =
    div []
        [ textarea [ onInput SetText, value model.text ] []
        , div
            []
            [ text model.text ]
        ]
