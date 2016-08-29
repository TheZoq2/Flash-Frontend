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
    | AddTagManager


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        TagListMsg id tagListMsg ->
            ({model | tagManagerList = (List.map tagManagerUpdateHelper model.tagManagerList)}, Cmd.none)

        TagListToggle id enable ->
            (model, Cmd.none)

        AddTagManager ->
            (model, Cmd.none)


tagManagerUpdateHelper : Int -> TagListManager.Msg -> TagListManagerContainer -> TagListManagerContainer
tagManagerUpdateHelper targetId msg container =
    let 
        manager = container.manager
    in
        {container | manager = (if targetId == container.id then manager else manager) }






-- VIEW

view : Model -> Html Msg
view model =
    div [] 
    (
        [
            button [onClick AddTagManager] [text "Add new tag list"]

        ] ++ 
        List.map viewFromTagManager model.tagManagerList
    )


viewFromTagManager : TagListManagerContainer -> Html Msg
viewFromTagManager {id, manager, enable} =
    Html.App.map (TagListMsg id) (TagListManager.view manager)






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
