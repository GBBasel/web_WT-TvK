import String exposing (fromChar)
import Html.Attributes exposing (style)
import Html exposing (Html, button, div, p, table, td, text, tr)


-- MODEL

type alias Model = 
    { board : List Char
    , level : Int }

initialModel : Model
initialModel =
    { level = 1}

-- UPDATE

type Msg
    = ShowPattern
    | IncreaseDifficulty

update : Msg -> Model -> Model
update msg model =
    case msg of
        ShowPattern ->

        IncreaseDifficulty ->
            { model | level = model.level + 1 }


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ text "Merke dir das folgende Muster:"
        , p [] []
        , ul [1,2,3,4]
        , p [] []
        ]