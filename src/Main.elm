module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App

{-
    MODEL
-}

type alias Model =
    {

    }

initModel = {}

{- 
    UPDATE
-}
type Msg =
    DoNothing

update : Msg -> Model -> Model
update msg model =
    model

{-
    VIEW
-}


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "Scorecard" ]

        ]


main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }