module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App

{-
    MODEL
-}

type alias Model =
    {  course : Maybe Course,
       nameCandidate : String,
       holesToAdd : List Hole
    }

type alias Course =
    {
        name : String,
        holes : List Hole
    }

type alias Hole =
    {
        order : Int,
        par : Int
    }

initModel : Model
initModel = 
    {
        course = Nothing,
        nameCandidate = "",
        holesToAdd = []
    }

{- 
    UPDATE
-}
type Msg =
    AddHole Int

update : Msg -> Model -> Model
update msg model =
    case msg of 
        AddHole par
            -> let oldHoles = model.holesToAdd
                   newHole = Hole ((List.length model.holesToAdd) + 1) par
               in
                { model | holesToAdd = (newHole :: oldHoles)  }

{-
    VIEW
-}


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [class "row" ] 
              [ h1 [] [ text "Scorecard" ] ]
        , div [ class "row"] 
              [
                (renderScorecard model)
              ]
        ]

renderScorecard : Model -> Html Msg
renderScorecard model =
    case model.course of
        Just course -> 
            div [ ] [ text "Course is initialized" ]
        Nothing ->
            div [ class "row" ] 
                [ createCourseHeader
                , createCourseForm model
                ]

createCourseHeader : Html Msg
createCourseHeader = 
    h2 [] [text "Create a course"]

createCourseForm : Model -> Html Msg
createCourseForm model =
    Html.form [ class "form-inline" ]
        [ div [ class "form-group" ]
              [ label [ for "courseName" ] [ text "Name" ]
              , input [ type' "text", class "form-control", id "courseNameInput" ] [ text model.nameCandidate ]              
              ]
        , div [ class "form-group" ] 
              [ button [ class "btn", type' "button", onClick (AddHole 3) ] [ text "Add hole" ]
              ]
        , showHoles model 
        ]

showHoles : Model -> Html Msg
showHoles model =
    case List.isEmpty model.holesToAdd of 
        True -> 
            p [] [ text "No holes yet" ]
        False ->
            model.holesToAdd
            |> List.map renderHole
            |> ul []
            

renderHole : Hole -> Html Msg
renderHole hole =
    li [] 
       [ div [ class "form-group" ]
             [
                label [ ] [ text ("Hole #" ++ (toString hole.order ) ++ ", par ") ]
             ,  input [ type' "number", value (toString hole.par) ] []
             ]
       ]

main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }