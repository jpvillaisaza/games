module Scorekeeper exposing (Model, Msg, init, update, view)

-- elm/browser
import Browser

-- elm/html
import Html exposing (Html)
import Html.Attributes
import Html.Events


type alias Player =
  { name : String
  }

type View
  = Config
  | Game
  | Summary

type alias Model =
  { score : List (Maybe Int)
  , scores : List (List Int)
  , player1 : Player
  , player2 : Player
  , currentView : View
  , showTotal : Bool
  }

type Msg
  = AddScore
  | UpdateScore Int String
  | NewGame
  | UpdatePlayer1Name String
  | UpdatePlayer2Name String
  | UpdateView View
  | ToggleShowTotal

init : Model
init =
  { score = List.repeat 2 Nothing
  , scores = []
  , player1 = { name = ("Player 1") }
  , player2 = { name = ("Player 2") }
  , currentView = Game
  , showTotal = False
  }

main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddScore ->
      if List.all isJust model.score
        then
          { model
          | score = List.repeat 2 Nothing
          , scores = List.append model.scores [catMaybes model.score]
          }
        else
          model
    UpdateScore i scoreS ->
      case String.toInt scoreS of
        Just score ->
          { model | score = List.indexedMap (\j s -> if i == j then Just score else s) model.score }
        Nothing ->
          model
    NewGame ->
      { model | score = List.repeat 2 Nothing, scores = [], currentView = Game }
    UpdatePlayer1Name name ->
      { model | player1 = { name = name } }
    UpdatePlayer2Name name ->
      { model | player2 = { name = name } }
    UpdateView currentView ->
      { model | currentView = currentView }
    ToggleShowTotal ->
      { model | showTotal = not model.showTotal }

view : Model -> Html Msg
view model =
  case model.currentView of
    Config ->
      viewConfig model
    Game ->
      viewGame model
    Summary ->
      viewSummary model

viewConfig : Model -> Html Msg
viewConfig model =
  Html.form
    [ Html.Events.onSubmit (UpdateView Game)
    ]
    [ Html.input
        [ Html.Events.onInput UpdatePlayer1Name
        , Html.Attributes.required True
        , Html.Attributes.type_ "text"
        , Html.Attributes.value model.player1.name
        ]
        []
    , Html.input
        [ Html.Events.onInput UpdatePlayer2Name
        , Html.Attributes.required True
        , Html.Attributes.type_ "text"
        , Html.Attributes.value model.player2.name
        ]
        []
    , Html.div []
        [ Html.input
            [ Html.Events.onCheck (always ToggleShowTotal)
            , Html.Attributes.checked model.showTotal
            , Html.Attributes.id "showTotal"
            , Html.Attributes.type_ "checkbox"
            ]
            []
        , Html.label
            [Html.Attributes.for "showTotal"]
            [Html.text "Show total?"]
        ]
    , Html.button
        []
        [Html.text "Back"]
    ]

viewGame : Model -> Html Msg
viewGame model =
  Html.div []
    [ Html.button
        [Html.Events.onClick (UpdateView Config)]
        [Html.text "Settings"]
    , Html.table []
        [ Html.thead []
            [ Html.tr
                [ Html.Attributes.class "row"
                ]
                [ Html.th [Html.Attributes.scope "col"] [Html.text "Round"]
                , Html.th [Html.Attributes.scope "col"] [Html.text model.player1.name]
                , Html.th [Html.Attributes.scope "col"] [Html.text model.player2.name]
                ]
            ]
        , Html.tbody []
            (List.indexedMap (viewRow << Just) model.scores)
        , Html.tfoot []
            (if model.showTotal then
              [ viewRow Nothing (List.map List.sum (transpose model.scores)) ]
            else [])
        ]
    , Html.form [Html.Events.onSubmit AddScore]
        [ Html.table []
            [ Html.tbody []
                [ Html.tr
                    [ Html.Attributes.class "row"
                    ]
                    (Html.th [Html.Attributes.scope "row"] [Html.text (String.fromInt (List.length model.scores + 1))]
                      :: List.map (Html.td [] << List.singleton) (List.indexedMap viewScore model.score))
                ]
            ]
        , Html.button [] [Html.text "Add"]
        ]
    , Html.button [Html.Events.onClick (UpdateView Summary)]
        [ Html.text "Finish game" ]
    ]

viewScore : Int -> Maybe Int -> Html Msg
viewScore i ms =
  Html.input
    [ Html.Events.onInput (UpdateScore i)
    -- , Html.Attributes.autofocus (i == 0)
    , Html.Attributes.required True
    , Html.Attributes.type_ "number"
    , Html.Attributes.value (Maybe.withDefault "" (Maybe.map String.fromInt ms))
    ]
    []

viewRow : Maybe Int -> List Int -> Html msg
viewRow mi score =
  Html.tr []
    (Html.th [Html.Attributes.scope "row"] [Html.text (Maybe.withDefault "" (Maybe.map (String.fromInt << (+) 1) mi))]
      :: List.map viewRowVal score)

viewRowVal : Int -> Html msg
viewRowVal score =
    Html.td [] [Html.text (String.fromInt score)]

viewSummary : Model -> Html Msg
viewSummary model =
  Html.div []
    [ Html.table []
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] []
                , Html.th [] [Html.text model.player1.name]
                , Html.th [] [Html.text model.player2.name]
                ]
            ]
        , Html.tfoot []
            [ if not (List.isEmpty model.scores) then
                viewRow Nothing (List.map List.sum (transpose model.scores))
              else
                viewRow Nothing (List.map (always 0) model.score)
            ]
        ]
    , Html.button
        [ Html.Events.onClick (UpdateView Game)
        ]
        [ Html.text "Continue playing"
        ]
    , Html.button
        [ Html.Events.onClick NewGame
        ]
        [Html.text "New game"]
    ]

transpose : List (List a) -> List (List a)
transpose list =
  case list of
    [] ->
      []
    [] :: xss ->
      transpose xss
    (x :: xs) :: xss ->
      (x :: heads xss)
        :: transpose (xs :: tails xss)

heads : List (List a) -> List a
heads =
  catMaybes << List.map List.head

tails : List (List a) -> List (List a)
tails =
  catMaybes << List.map List.tail

isJust : Maybe a -> Bool
isJust m =
  case m of
    Just _ ->
      True
    Nothing ->
      False

catMaybes : List (Maybe a) -> List a
catMaybes l =
  case l of
    [] ->
      []
    mx :: xs ->
      case mx of
        Just x ->
          x :: catMaybes xs
        Nothing ->
          catMaybes xs
