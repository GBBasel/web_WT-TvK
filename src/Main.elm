import Html exposing (Main)


-- MODEL

type alias Model = 
    { board: List Char}

-- UPDATE

type MZ
    = Zeigen


update : Msg -> Model -> Model
update msg MZ =
  case msg of
    


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ text "Merke dir das folgende Muster:"]
        ,p [] []
        ,