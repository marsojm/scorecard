module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import String exposing (toInt)

{-
    MODEL
-}

parForHoleDefault : String
parForHoleDefault = "3"

type alias Model =
    {  course : Maybe Course,
       nameCandidate : String,
       holesToAdd : List Hole,
       parForHole : String,
       error : Maybe String,
       gameView : Bool
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
        holesToAdd = [],
        parForHole = parForHoleDefault,
        error = Nothing,
        gameView = False
    }

{- 
    UPDATE
-}
type Msg =
    AddHole
    | InputPar String
    | InputCourseName String
    | CreateCourse

update : Msg -> Model -> Model
update msg model =
    case msg of 
        AddHole
            -> updateParHolesToAdd model
            
                
        InputCourseName name
            -> { model | nameCandidate = name }
        InputPar input
            -> { model | parForHole = input }
        CreateCourse
            ->  if (String.length model.nameCandidate) > 0 && (List.length model.holesToAdd > 0) then 
                    { model | gameView = True, error = Nothing }
                else 
                    { model | error = Just "Course must have a name and at least one hole!" }

parIsValid : String -> Bool
parIsValid input =
    case String.toInt input of
        Ok val 
            -> True 
        _   -> False

updateParHolesToAdd : Model -> Model
updateParHolesToAdd model =
    case String.toInt model.parForHole of
        Ok val 
            -> if val > 0 then
                 let oldHoles = model.holesToAdd
                     newHole = Hole (nextIdForHole model) val
                 in
                { model | 
                  parForHole = parForHoleDefault
                , error = Nothing
                , holesToAdd = (newHole :: oldHoles) }
               else 
                { model | error = Just "Par value must be greter than zero!" }
        _ -> 
            { model | error = Just "Par value must be greter than zero!" }

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
                renderScorecard model
              ]
        , p [] [ text (toString model)] -- for dev purposes
        ]

renderScorecard : Model -> Html Msg
renderScorecard model =
    case model.gameView of
        True -> 
            div [ ] [ text "Course is initialized" ]
        _ ->
            div [ class "row" ] 
                [ createCourseHeader
                , createCourseForm model
                ]

createCourseHeader : Html Msg
createCourseHeader = 
    h2 [] [text "Create a course"]

createCourseForm : Model -> Html Msg
createCourseForm model =
    Html.form [ class "form-horizontal" ]
        [ formErrors model
        , courseName model
        , addHoleForm model
        , button [ type' "button", class "btn btn-primary", onClick CreateCourse ] [ text "Create" ]
        , showHoles model 
        ]

formErrors : Model -> Html Msg
formErrors model =
    case model.error of
        Just err
            -> div [class "alert alert-danger" ] 
                   [ 
                    p [] [ text err ]
                   ]
        _ -> div [] []
    

addHoleForm : Model -> Html Msg
addHoleForm model =
    div [ class "form-group" ] 
        [ label [ class "control-label col-sm-2" ] [ text ("Hole #" ++ (toString <| nextIdForHole model ) ++ ", Par: ") ]
        , div [ class "col-sm-3" ] 
              [ input [ type' "text", class "form-control", onInput InputPar ] [ text model.parForHole ]
              ] 
        , button [ class "btn", type' "button", onClick AddHole ] [ text "Add hole" ]
        ]
         


nextIdForHole : Model -> Int
nextIdForHole model = 
    (List.length model.holesToAdd) + 1


courseName : Model -> Html Msg
courseName model =
    div [ class "form-group" ]
        [ label [ class "control-label col-sm-2" ] [ text "Name" ]
        , div [class "col-sm-3" ] 
              [ input [ type' "text", class "form-control", onInput InputCourseName ] [ text model.nameCandidate ]
              ]               
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
    li [] [ text ("Hole #" ++ (toString hole.order ) ++ ", par " ++ (toString hole.par)) ]

main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }