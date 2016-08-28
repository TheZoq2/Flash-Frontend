import TagListManager
import Style

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (..)
import Json.Encode
import Http
import Task


-- MODEL

type alias TagListManagerContainer =
    {
        id: Int,
        manager: TagListManager.Model,
        enable: Bool
    }

type alias Model =
    {
        tagManagerList: List TagListManagerContainer
    }

init: (Model, Cmd Msg)
init = 
    (Model [], Cmd.none)





-- UPDATE

type Msg
    = TagListMsg Int TagListManager.Msg
    | TagListToggle Int Bool


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)







-- VIEW

view : Model -> Html Msg
view model =
    div [] 
    [

    ] ++ 
    List.map 
        (\manager -> Html.App.map (TagListMsg 1) (TagListManager.view manager)) 
        model.tagManagerList.manager







-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none




-- Main
main =
    Html.App.program
        {
            init = init,
            update = update,
            view = view,
            subscriptions = subscriptions
        }
