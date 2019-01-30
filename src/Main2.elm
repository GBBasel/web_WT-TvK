import Html exposing (..)


-- MODEL

type alias Model =
    { board: List Char
    , level : Int }


-- UPDATE

type Msg
    = ShowPattern

update : Msg -> Model -> Model
update msg model =
  case msg of
   ShowPattern ->
        


-- VIEW

view : Model -> Html Msg
view model =
      div []
        [ text "Merke dir das folgende Muster:"
        , p [] []
        , ul [ShowPattern]
        , p [] []
        ]