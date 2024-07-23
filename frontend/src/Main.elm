module Main exposing (main)

import Browser
import Html exposing (Html, div, button, input, text, img)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

-- MODEL

type alias Model =
    { selectedImage : Maybe String }

init : Model
init =
    { selectedImage = Nothing }


-- UPDATE

type Msg
    = OpenFileDialog
    | FileSelected String

update : Msg -> Model -> Model
update msg model =
    case msg of
        OpenFileDialog ->
            model

        FileSelected file ->
            { model | selectedImage = Just file }


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "file", accept "image/*", onInput FileSelected ] []
        , case model.selectedImage of
            Nothing ->
                text "No image selected"

            Just image ->
                div []
                    [ text "Selected image: "
                    , img [ src image, style "max-width" "600px", style "max-height" "600px" ] []
                    ]
        ]

main = Browser.sandbox { init = init, update = update, view = view }
