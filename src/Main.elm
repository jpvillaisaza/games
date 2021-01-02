module Main exposing (main)

-- elm/browser
import Browser

-- elm/html
import Html exposing (Html)
import Html.Attributes
import Html.Events

-- games
import Scorekeeper


type alias Model =
  { page : Page
  , scorekeeper : Scorekeeper.Model
  }

type Msg
  = UpdatePage Page
  | UpdateScorekeeper Scorekeeper.Msg

type Page
  = Home
  | Scorekeeper

init : () -> (Model, Cmd Msg)
init _ =
  ( { page = Home
    , scorekeeper = Scorekeeper.init
    }
  , Cmd.none
  )

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , update = update
    , subscriptions = always Sub.none
    , view = view
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdatePage page ->
      ( { model
        | page = page
        }
      , Cmd.none)
    UpdateScorekeeper scorekeeperMsg ->
      ( { model
        | scorekeeper = Scorekeeper.update scorekeeperMsg model.scorekeeper
        }
      , Cmd.none
      )

view : Model -> Browser.Document Msg
view model =
  { body =
      [ Html.main_ []
          [ case model.page of
              Home ->
                Html.button
                  [Html.Events.onClick (UpdatePage Scorekeeper)]
                  [Html.text "Scorekeeper"]
              Scorekeeper ->
                Html.div
                  [ Html.Attributes.class "game" ]
                  [ Html.map UpdateScorekeeper
                      (Scorekeeper.view model.scorekeeper)
                  ]
          ]
      ]
  , title =
      case model.page of
        Home ->
          "Games"
        Scorekeeper ->
          "Scorekeeper | Games"
  }
