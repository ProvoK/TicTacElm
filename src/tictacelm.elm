import Array exposing (..)
import Browser
import Html.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

main =
  Browser.sandbox { init = init, update = update, view = view }


type Player = P1 | P2 | Nobody

playerToString: Player -> String
playerToString player =
    case player of
        P1 -> "RED"
        P2 -> "GREEN"
        Nobody -> "-"

playerToColor: Player -> String
playerToColor player =
    case player of
        P1 -> "red"
        P2 -> "green"
        Nobody -> "grey"


type alias Model =
    { board: List Player, currentPlayer: Player, statusBarMsg : String, gameFinished : Bool }

init : Model
init = { board = List.repeat 9 Nobody, currentPlayer = P1, statusBarMsg = "", gameFinished = False }

type Msg = SetTile Player Int | NewGame

setTilePlayer : Player -> Int -> Model -> Model
setTilePlayer player pos model =
    { model | board = ( List.take pos model.board ) ++ [player] ++ (List.drop (pos + 1) model.board) }

setNextPlayer : Model -> Model
setNextPlayer model =
    case model.currentPlayer of
        P1 -> { model | currentPlayer = P2 }
        P2 -> { model | currentPlayer = P1 }
        Nobody -> model


canPlace: Model -> Player -> Int -> Bool
canPlace model player pos =
    Array.fromList model.board
    |> Array.get pos
    |> Maybe.withDefault Nobody
    |> \p -> p == Nobody

takeByIndex: List Int -> a -> List a -> List a
takeByIndex indexes default list =
    list
    |> Array.fromList
    |> (\l -> List.map (\n -> Array.get n l) indexes)
    |> List.map (Maybe.withDefault default)

winSlices: Model -> List (List Player)
winSlices model =
    [ takeByIndex [0, 1, 2] Nobody model.board
    , takeByIndex [3, 4, 5] Nobody model.board
    , takeByIndex [6, 7, 8] Nobody model.board
    , takeByIndex [0, 3, 6] Nobody model.board
    , takeByIndex [2, 5, 8] Nobody model.board
    , takeByIndex [0, 4, 8] Nobody model.board
    , takeByIndex [2, 4, 6] Nobody model.board
    , takeByIndex [1, 4, 7] Nobody model.board]


checkWinConditions: Model -> Model
checkWinConditions model =
    model
    |> winSlices
    |> List.map (List.all (\t -> t == model.currentPlayer))
    |> List.any (\v -> v == True)
    |> \b ->
        if b == True then
            { model | gameFinished = True, statusBarMsg = playerToString model.currentPlayer ++ " WINS!!!" }
        else
            model

checkEndConditions: Model -> Model
checkEndConditions model =
    model.board
    |> List.all (\t -> t /= Nobody)
    |> \b ->
        if b == True then
            { model | gameFinished = True, statusBarMsg = "Nobody won :sadpepe:" }
        else
            model

resetStatusBar: Model -> Model
resetStatusBar model = { model | statusBarMsg = "" }

update: Msg -> Model -> Model
update msg model =
    case msg of
        NewGame ->
            init
        SetTile player pos ->
            if model.gameFinished == True then
                model
            else if canPlace model player pos then
                model
                |> resetStatusBar
                |> (setTilePlayer player pos)
                |> checkEndConditions
                |> checkWinConditions
                |> setNextPlayer
            else
                { model | statusBarMsg = playerToString model.currentPlayer ++ " : you cannot place that here" }


cell : Model -> Int -> Player -> Html Msg
cell model n txt =
    button
    [ onClick (SetTile model.currentPlayer n)
    , style "background-color" (playerToColor txt)
    , style "height" "60px"
    , style "outline" "0"
    ] []

view : Model -> Html Msg
view model =
    div []
    [ div [ style "grid-template-columns" "60px 60px 60px", style "display" "grid" ] (List.indexedMap (cell model) model.board)
    , div [] [ text model.statusBarMsg ]
    , div [ hidden (not model.gameFinished) ] [ button [ onClick NewGame ] [ text "NewGame" ]]
    ]
