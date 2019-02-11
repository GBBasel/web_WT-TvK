module Main exposing (..)

import Browser
import String exposing (fromChar)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
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
  , Cmd.none )

-- UPDATE

type Msg
    = Roll
    | GeneratePattern (List Int)
    | IncreaseDifficulty
    | AskforPattern
    


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model, Random.generate GeneratePattern (Random.list model.difficulty ( Random.int 0 9)))
    GeneratePattern newpattern ->
        ( { model | pattern = newpattern }, Cmd.none)
    IncreaseDifficulty ->
        ( { model | difficulty = model.difficulty + 1}, Cmd.none)
    AskforPattern ->
        

     
      

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
        , text <| String.join "" <| List.map String.fromInt model.pattern
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