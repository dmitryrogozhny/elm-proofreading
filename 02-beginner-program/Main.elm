import Html exposing (div, text, beginnerProgram)

main = beginnerProgram { model = Model, view = view, update = update }

-- MODEL
type alias Model = {}

-- UPDATE
type Msg = None


update msg model =
    case msg of
        None ->
            model


-- VIEW
view model =
    div [] [ text "Hello, World!" ]
