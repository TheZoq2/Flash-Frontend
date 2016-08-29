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
initContainer : Int -> TagListManagerContainer
initContainer id =
    {
        id = id,
        manager = TagListManager.init,
        enable = True
    }


type alias Model =
    {
        tagManagerList: List TagListManagerContainer,
        nextId: Int
    }

init: (Model, Cmd Msg)
init = 
    (Model [] 0, Cmd.none)





-- UPDATE

type Msg
    = TagListMsg Int TagListManager.Msg
    | TagListToggle Int Bool
    | AddTagManager


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        TagListMsg id tagListMsg ->
            ({model | tagManagerList = List.map (tagManagerUpdateHelper id tagListMsg) model.tagManagerList}, Cmd.none)

        TagListToggle id enable ->
            (model, Cmd.none)

        AddTagManager ->
            let
                newContainerList = model.tagManagerList ++ [initContainer model.nextId]

                nextId = model.nextId + 1
            in
                ({model | tagManagerList = newContainerList, nextId = nextId}, Cmd.none)


tagManagerUpdateHelper : Int -> TagListManager.Msg -> TagListManagerContainer -> TagListManagerContainer
tagManagerUpdateHelper targetId msg container =
    let 
        manager = container.manager
    in
        {container | manager = (if targetId == container.id then TagListManager.update msg manager else manager) }






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
