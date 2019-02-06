module Main exposing (..)

import Browser
import String exposing (fromChar)
import Html.Attributes exposing (style)
import Html exposing (Html, button, div, p, table, td, text, tr)
import Random exposing (int)

-- MODEL

type alias Model = 
    { difficulty : Int
    , pattern : List Int }

initialModel : Model
initialModel =
    { difficulty = 1
    , pattern = []}
    

init : () -> (Model, Cmd Msg)
init _ =
  ( initialModel
  , Cmd.none
  )

-- UPDATE

type Msg
    = Roll
    | GeneratePattern (List Int)
    | IncreaseDifficulty


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Random.generate GeneratePattern (Random.list 10 ( Random.int 1 6))
      )
    GeneratePattern newpattern ->
     ( { model | pattern = newpattern }, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ text "Merke dir das folgende Muster:"
        , p [] []
        , text <| string.fromInt
        , p [] []
        , button [ onClick Roll ] [ text "Roll" ]
        ]

-- MAIN
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }