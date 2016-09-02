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
       holes : List Hole,
       parForHole : String,
       error : Maybe String,
       gameView : Bool,
       players : List Player
    }

type alias ThrowResult = 
    { holeId : Int,
      throws : Int
    }

type alias Player = 
    {
        id : Int,
        name : String,
        results : List ThrowResult
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

{-initModel : Model
initModel = 
    {
        course = Nothing,
        nameCandidate = "",
        holes = [],
        parForHole = parForHoleDefault,
        error = Nothing,
        gameView = False
    }
-}
initModel : Model
initModel = 
    {
        course = Nothing,
        nameCandidate = "My Course",
        holes = [ Hole 1 3
                , Hole 2 3
                , Hole 3 4
                ],
        parForHole = parForHoleDefault,
        error = Nothing,
        gameView = True,
        players = [
            Player 1 "James" [ ThrowResult 1 4, ThrowResult 2 2, ThrowResult 3 4]
            ,Player 2 "Dick" [ ThrowResult 1 1, ThrowResult 2 1]
        ]
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
            ->  if (String.length model.nameCandidate) > 0 && (List.length model.holes > 0) then 
                    { model | gameView = True, error = Nothing, holes = List.sortBy .order model.holes  }
                else 
                    { model | error = Just "Course must have a name and at least one hole!" }

updateParHolesToAdd : Model -> Model
updateParHolesToAdd model =
    case String.toInt model.parForHole of
        Ok val 
            -> if val > 0 then
                 let oldHoles = model.holes
                     newHole = Hole (nextIdForHole model) val
                 in
                { model | 
                  parForHole = parForHoleDefault
                , error = Nothing
                , holes = (newHole :: oldHoles) }
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
            div [] 
                [ div [ class "row" ] [h3 [] [ text ("Course: " ++ model.nameCandidate) ]]
                , table [ class "table table-bordered" ] 
                        [ renderTableHeader model
                        , renderTableBody model
                        ]
                ]
        _ ->
            div [ class "row" ] 
                [ createCourseHeader
                , createCourseForm model
                ]

{-
    Game view
-}

renderTableHeader : Model -> Html Msg
renderTableHeader model =
    thead [] [ renderHoleNumbers model
             , renderParRow model 
             ]

renderParRow : Model -> Html Msg
renderParRow model =
    model.holes
    |> List.map (\h -> th [] [ text <| toString h.par ] )
    |> (\lst -> ((th [] [ text "Players" ]) :: lst) ++ [(th [] [ text "Total" ])] )
    |> tr [] 

renderHoleNumbers :Model -> Html Msg
renderHoleNumbers model = 
    model.holes
    |> List.map (\h -> th [] [ text <| toString h.order ] )
    |> (\lst -> ((th [] []) :: lst) ++ [(th [] [])] ) 
    |> tr []

renderTableBody : Model -> Html Msg
renderTableBody model =
          renderPlayers model
          
renderPlayers : Model -> Html Msg
renderPlayers model =
    model.players
    |> List.map (\p -> renderPlayer model.holes p)
    |> tbody []

renderPlayer : List Hole -> Player -> Html Msg
renderPlayer holes player =
    ( (playerNameCell player) :: (scores player holes) ) 
    |> List.reverse 
    |> (\lst -> (td [] [ text <| toString <| calculateTotal player holes ]) :: lst)
    |> List.reverse 
    |> tr []   
     
calculateTotal : Player -> List Hole -> Int
calculateTotal player holes =
    let results = player.results
    in 
        calculateTotalsRec 0 results holes

calculateTotalsRec : Int -> List ThrowResult -> List Hole -> Int
calculateTotalsRec acc results holes =
        case results of
            head::rest
                -> calculateTotalsRec (acc + (calculateScoreForThrow head holes ) ) (rest) holes 
            [] 
                -> acc

calculateScoreForThrow : ThrowResult -> List Hole -> Int
calculateScoreForThrow throwRes holes =
    let hole = List.head (List.filter (\h -> throwRes.holeId == h.order) holes)
    in
        case hole of
            Just v
                -> throwRes.throws - v.par
            _ 
                -> 0

playerNameCell : Player -> Html Msg
playerNameCell player =
    td [] [text player.name]

scores : Player -> List Hole -> List (Html Msg)
scores player holes =
    List.map (\h -> scoreCell player h) holes

scoreCell : Player -> Hole -> Html Msg
scoreCell player hole =
    let throwResult = List.head (List.filter (\tr -> tr.holeId == hole.order) player.results)
    in 
        case throwResult of
            Just x
                -> td [] [ text (toString (x.throws - hole.par)) ]
            Nothing 
                -> td [] [text "-"] 
    

{-
    Create course view
-}
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
    (List.length model.holes) + 1


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
    case List.isEmpty model.holes of 
        True -> 
            p [] [ text "No holes yet" ]
        False ->
            model.holes
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