port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (split)
import Styles
import Styles exposing (containerImageStyle)
import Components exposing (..)
import Types exposing (..)
-- Ports for reading file data
port requestFileRead : String -> Cmd msg
port receiveFileDataUrl : (String -> msg) -> Sub msg

-- Ports for send data to ImageApi
port sendFile : String -> Cmd msg
port receiveAscii : (String -> msg) -> Sub msg
port sendScaleFactor : Int -> Cmd msg


init : Model
init =
    { selectedImage = Nothing 
    , asciiArt = Nothing 
    , scaleFactor = 32 }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        OpenFileDialog ->
            (model, Cmd.none)
        FileSelected fileId ->
            let
                getUrlData = requestFileRead fileId
                sendToApi = sendFile fileId 
            in
            (model, Cmd.batch [ getUrlData, sendToApi ])

        ReceiveFileDataUrl dataUrl ->
            ({ model | selectedImage = Just dataUrl }, Cmd.none)

        SetAsciiArt asciiArt ->
            ({ model | asciiArt = Just asciiArt }, Cmd.none)
        ValueChanged newValue ->
            let
                updatedScaleFactor = Maybe.withDefault model.scaleFactor (String.toInt newValue)
                sendScaleFactorCmd = sendScaleFactor updatedScaleFactor
                
            in
            ({ model | scaleFactor = updatedScaleFactor }, sendScaleFactorCmd)

view : Model -> Html Msg
view model =
    div Styles.mainDiv
        [ headerComponent model
        , imageSelectionComponent model
        , rangeInputComponent model
        , fileInputComponent
        , asciiArtPreviewComponent model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveFileDataUrl ReceiveFileDataUrl
        , receiveAscii SetAsciiArt
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (init, Cmd.none)  
        , update = update
        , view = view
        , subscriptions = subscriptions
        }