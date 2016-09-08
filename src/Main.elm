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
       players : List Player,
       scoreToEdit : Maybe (Player, Hole),
       newScoreValue : String,
       playerToAdd : String
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

initModel : Model
initModel = 
    {
        course = Nothing,
        nameCandidate = "",
        holes = [],
        parForHole = parForHoleDefault,
        error = Nothing,
        gameView = False,
        players = [],
        scoreToEdit = Nothing,
        newScoreValue = "",
        playerToAdd = ""
    }

{- 
    UPDATE
-}
type Msg =
    AddHole
    | InputPar String
    | InputCourseName String
    | CreateCourse
    | ToggleEdit Player Hole
    | SaveScore
    | InputScoreToEdit String
    | InputPlayerName String
    | AddPlayer
    | DecPar Int
    | IncPar Int

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
        ToggleEdit player hole
            -> handleToggleEdit model player hole
        InputScoreToEdit input 
            -> { model | newScoreValue = input }
        SaveScore 
            -> if model.newScoreValue == "" || model.newScoreValue == "-" then
                    { model | scoreToEdit = Nothing, error = Nothing }
               else 
                    case String.toInt model.newScoreValue of
                        Ok val -> 
                            if val > 0 then 
                                { model | 
                                  newScoreValue = ""
                                  , scoreToEdit = Nothing
                                  , players = updatePlayerScore model val  
                                } 
                            else 
                                { model | error = Just "Score must be greater than zero" }
                        Err _ 
                        -> { model | error = Just "Score must be a number greater than zero" }
        InputPlayerName name 
            -> { model | playerToAdd = name }
        AddPlayer
            -> case String.trim model.playerToAdd of 
                ""
                    -> model 
                name 
                    -> let newId = (List.length model.players) + 1
                       in 
                        { model | players = (Player newId name [])::model.players, playerToAdd = "" }
        DecPar holeId 
            -> let newHoles = List.map (\hole -> if hole.order == holeId then decrementPar hole else hole ) model.holes
               in 
                { model | holes = newHoles }
        IncPar holeId 
            ->  let newHoles = List.map (\hole -> if hole.order == holeId then incrementPar hole else hole ) model.holes
               in 
                { model | holes = newHoles }

decrementPar : Hole -> Hole
decrementPar hole =
    if hole.par <= 1 then hole 
    else 
      { hole | par = hole.par - 1 }

incrementPar : Hole -> Hole
incrementPar hole = 
      { hole | par = hole.par + 1 }

updatePlayerScore : Model -> Int -> List Player 
updatePlayerScore model val =
    case model.scoreToEdit of 
        Just (player, hole)
            -> let throwResults = List.filter (\throwResult -> throwResult.holeId /= hole.order) player.results
               in 
                 let newPlayer = { player | results = (ThrowResult hole.order val)::throwResults }
                     oldPlayers = List.filter (\p -> p.id /= player.id) model.players
                 in 
                     newPlayer::oldPlayers  
        _           
         -> model.players
                    
handleToggleEdit : Model -> Player -> Hole -> Model
handleToggleEdit model player hole =
    case model.scoreToEdit of
        Nothing 
            -> { model | scoreToEdit = Just (player, hole), error = Nothing }
        Just (oldPlayer, oldHole)
            -> if (oldPlayer == player && oldHole == hole) then 
                { model | scoreToEdit = Nothing, error = Nothing }
               else 
                { model | scoreToEdit = Just (player, hole), error = Nothing }

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
        [ headerView model
        , formErrors model
        , mainView model   
        ]

mainView : Model -> Html Msg
mainView model =
    case model.gameView of
        True -> 
                div [  ] 
                    [ 
                     renderScoreEditForm model
                    , gameTable model
                    , addPlayerForm model
                    ]
                
        _ ->
            div [ class "row" ] 
                [ createCourseView model ]

{-
    Game view
-}

gameTable : Model -> Html Msg
gameTable model =
    div [ class "row" ] [
        table [ class "table table-bordered" ] 
            [ renderTableHeader model
            , renderTableBody model
            ]
    ]

addPlayerForm : Model -> Html Msg
addPlayerForm model =
    div [ class "row" ]
        [ div [ class "panel panel-default" ]
              [ div [ class "panel-heading" ] [ text "Add a player" ]
              , div [ class "panel-body" ] 
                    [ Html.form [ class "form-inline", onSubmit AddPlayer ] 
                                [ div [ class "form-group" ] 
                                                    [ label [ class "col-xs-4" ] [ text "Name: " ] 
                                                    , input [ class "col-xs-6", onInput InputPlayerName, type' "text", value model.playerToAdd ] []
                                                    , button [ type' "submit"] [ text "Add"]
                                                    ]
                                ]
                    ] 
              ] 
        ]

renderScoreEditForm : Model -> Html Msg
renderScoreEditForm model = 
    case model.scoreToEdit of
        Nothing 
            -> div [] []
        Just (player, hole)
            ->  div [ class "row" ] 
                    [ div [ class "panel panel-default" ]
                          [ div [ class "panel-heading" ] [ text "Edit throws" ]
                          , div [ class "panel-body" ] 
                                [
                                    Html.form [ class "form-inline", onSubmit SaveScore ] 
                                                [ div [ class "form-group" ] 
                                                    [ label [ class "col-xs-4" ] [ text player.name ] 
                                                    , input [ class "col-xs-2", onInput InputScoreToEdit, type' "text", placeholder (scoreForHole hole player)] []
                                                    , button [ type' "submit"] [ text "Save throws"]
                                                    , span [ class "help-block" ] [ text "Remember, the input is the number of throws, not the final score. The score is calculated after the number of throws is given." ]
                                                    ]
                                                ]
                                ]
                          ] 
                    ]

scoreForHole : Hole -> Player -> String
scoreForHole hole player = 
    let res = List.filter (\r -> r.holeId == hole.order) player.results 
    in  
        case res of
            x::xs 
                -> 
                   toString x.throws
            _ 
                -> 
                    "-" 


renderTableHeader : Model -> Html Msg
renderTableHeader model =
    thead [] [ renderHoleNumbers model
             , renderParRow model 
             ]

renderParRow : Model -> Html Msg
renderParRow model =
    model.holes
    |> List.map (\h -> th [ class "bg-warning" ] [ text <| "Par " ++ toString h.par ] )
    |> (\lst -> ((th [ class "bg-warning" ] [ text "Players" ]) :: lst) ++ [(th [ class "bg-warning" ] [ text "Total" ])] )
    |> tr [] 

renderHoleNumbers :Model -> Html Msg
renderHoleNumbers model = 
    model.holes
    |> List.map (\h -> th [ class "bg-primary" ] [ text <| toString h.order ] )
    |> (\lst -> ((th [ class "bg-primary" ] []) :: lst) ++ [(th [ class "bg-primary" ] [])] ) 
    |> tr []

renderTableBody : Model -> Html Msg
renderTableBody model =
          renderPlayers model
          
renderPlayers : Model -> Html Msg
renderPlayers model =
    model.players
    |> List.sortBy .id
    |> List.map (\p -> renderPlayer model.holes p model.scoreToEdit)
    |> tbody []

renderPlayer : List Hole -> Player -> Maybe (Player, Hole) -> Html Msg
renderPlayer holes player scoreToEdit =
    ( (playerNameCell player) :: (scores player holes scoreToEdit) ) 
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

scores : Player -> List Hole -> Maybe (Player, Hole) -> List (Html Msg)
scores player holes scoreToEdit =
    List.map (\h -> scoreCell player h scoreToEdit) holes

scoreCell : Player -> Hole -> Maybe (Player, Hole) -> Html Msg
scoreCell player hole scoreToEdit =
    let throwResult = List.head (List.filter (\tr -> tr.holeId == hole.order) player.results)
        cStyle = cellStyle player hole scoreToEdit  
    in 
        case throwResult of
            Just x
                -> td [ class cStyle, onClick (ToggleEdit player hole)] 
                      [ text ((toString x.throws) 
                             ++ " (" ++ 
                             (toStringScore (x.throws - hole.par) )  ++ ")") ]
            Nothing 
                -> td [ class cStyle, onClick (ToggleEdit player hole) ] [text "-"] 

toStringScore : Int -> String
toStringScore score =
    if score >= 0 then  "+" ++ (toString score)
    else (toString score)

cellStyle : Player -> Hole -> Maybe (Player, Hole) -> String
cellStyle player hole scoreToEdit =
    case scoreToEdit of
        Just (p,h)
            -> if h == hole && p == player then 
                "info"
               else 
                ""
        Nothing
            -> ""

{-
    Create course view
-}
headerView : Model -> Html Msg
headerView model =
    if model.gameView then 
        div [ class "page-header" ] 
            [ h1 [] [ text "Disc Golf Scorecard" ]
            , p [ class "lead"] [ text ("Course: " ++ model.nameCandidate) ]
            ]
             
    else
        div [ class "page-header" ] 
            [ h1 [] [ text "Disc Golf Scorecard" ]
            , p [ class "lead"] [ text "Create a course" ]
            ] 
        

createCourseView : Model -> Html Msg
createCourseView model =
    div [ class "row" ] 
        [ div [ class "col-md-3" ] [] 
        , div [ class "col-md-6" ] 
              [ courseName model
              , courseButtons model
              , showHoles model 
              ]
        , div [ class "col-md-3" ] [] 
        ]

courseButtons : Model -> Html Msg 
courseButtons model =
    div [ class "row button-row" ]
        [ addHoleForm model
        , createCourseButton
        , br [] []
        ]

addHoleForm : Model -> Html Msg
addHoleForm model =
        div [ class "col-md-6" ] [ button [ class "btn btn-info", type' "button", onClick AddHole ] [ text "Add hole" ] ]

createCourseButton : Html Msg
createCourseButton =   
    div [ class "col-md-6" ] [ button [ type' "button", class "btn btn-success pull-right", onClick CreateCourse ] [ text "Create" ] ]

formErrors : Model -> Html Msg
formErrors model =
    case model.error of
        Just err
            -> div [ class "row"]
                   [ div [class "alert alert-danger" ] [ p [] [ text err ] ]
                   ]
                
        _ -> div [] []
    


        
         


nextIdForHole : Model -> Int
nextIdForHole model = 
    (List.length model.holes) + 1


courseName : Model -> Html Msg
courseName model =
    div [ class "row" ] [
        div [ class "form-group" ] [
             div [] 
                [ input [ type' "text", class "form-control", placeholder "name of the course", onInput InputCourseName ] [ text model.nameCandidate ]
                ]               
            ]
    ]
    

showHoles : Model -> Html Msg
showHoles model =
    div [ class "row" ] [ div [ class "row"] [ holeList model ] ]
    

holeList : Model -> Html Msg 
holeList model =
    case List.isEmpty model.holes of 
        True -> 
            div [ class "alert alert-warning" ] [ text "No holes yet" ]
        False ->
            model.holes
            |> List.map renderHole
            |> (\lst -> listGroupHeader::lst)
            |> ul [ class "list-group" ]
           

listGroupHeader : Html Msg
listGroupHeader =
    li [ class "list-group-item active" ] 
       [ div [ class "row" ] 
             [ span [ class "strong col-md-1" ] [ text "#" ]
             , div [ class "col-md-1" ] []
             , span [ class "col-md-1 text-left" ] [ text "Par" ]
             , div [ class "col-md-1" ] [] 
             ] 
       ]

renderHole : Hole -> Html Msg
renderHole hole =
    li [ class "list-group-item" ] 
       [ holeForm hole ]

holeForm : Hole -> Html Msg
holeForm hole =
    div [ class "row" ] 
         [ span [ class "strong col-md-1" ] [ text ((toString hole.order) ++ ".") ]
         , button [ class "btn btn-danger col-md-1", type' "button", onClick (DecPar hole.order) ] [ text "-" ]
         , span [ class "strong col-md-1" ] [ text (toString hole.par) ]
         , button [ class "btn btn-success col-md-1", type' "button", onClick (IncPar hole.order) ] [ text "+" ]
         ]  
    

main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }