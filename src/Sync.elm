module Sync exposing (..)

import Html exposing (..)

{-|
  Type received from server with job updates
-}
type SyncStatus
    = GatheredData
    | SentToForeign Int
    | StartingToApplyChange Int
    | AddingChangeToDb Int
    | RemovingFile Int
    | AddingSyncpoint
    | Done
    | Error String


-- Model & init

type alias Model =
    { foreign_url: String
    , localJobId: Maybe Int
    , localStatus: Maybe SyncStatus
    , foreignStatus: Maybe SyncStatus
    }

init : (Model, Cmd Msg)
init =
    ( { foreign_url = ""
      , localJobId = Nothing
      , localStatus = Nothing
      , foreignStatus = Nothing
      }
    , Cmd.none
    )


-- Msg

type Msg
    = ForeignUrlSet String
    | StartClicked
    | JobIdReceived Int
    | LocalUpdate SyncStatus
    | ForeignUpdate SyncStatus



-- Update function

update : Msg -> Model -> (Model, Cmd Msg)
update msg model=
    (model, Cmd.none)



-- View function

view : Model -> Html Msg
view model =
    div []
        [ label [] [text "Foreign url"]
        , input [] []
        , button [] [text "go"]
        ]



-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Program

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
    
