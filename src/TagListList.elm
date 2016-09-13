module TagListList exposing(Model, Msg, init, update, getSelectedTags, view)

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
        nextTagListId: Int
    }


init : Model
init =
    Model [] 0





-- UPDATE

type Msg
    = TagListMsg Int TagListManager.Msg
    | AddTagManager
    | ToggleListManager Int
    | RemoveListManager Int


update : Msg -> Model -> Model
update msg model =
    case msg of 
        TagListMsg id tagListMsg ->
            {model | tagManagerList = List.map (tagManagerUpdateHelper id tagListMsg) model.tagManagerList}

        AddTagManager ->
            let
                newContainerList = model.tagManagerList ++ [initContainer model.nextTagListId]

                nextTagListId = model.nextTagListId + 1
            in
                {model | tagManagerList = newContainerList, nextTagListId = nextTagListId}
        ToggleListManager id ->
            let
                toggleWithId manager = 
                    if manager.id == id then
                        {manager | enable = manager.enable == False}
                    else
                        manager

                newContainerList = List.map toggleWithId model.tagManagerList

            in
                {model | tagManagerList = newContainerList}

        RemoveListManager id ->
            let
                newContainerList = List.filter (\manager -> manager.id /= id) model.tagManagerList
            in
                {model | tagManagerList = newContainerList }




-- UPDATE HELPER FUNCTIONS


tagManagerUpdateHelper : Int -> TagListManager.Msg -> TagListManagerContainer -> TagListManagerContainer
tagManagerUpdateHelper targetId msg container =
    let 
        manager = container.manager
    in
        {container | manager = (
                if targetId == container.id then 
                    TagListManager.update msg manager 
                else 
                    manager) }



getSelectedTags : Model -> List String
getSelectedTags model =
    let
        enabledTagContainers =
            List.filter (\manager -> manager.enable == True) model.tagManagerList

        managers = 
            List.map .manager enabledTagContainers
    in
        List.map TagListManager.getSelectedTags managers |> List.concat







-- VIEW

view : Model -> Html Msg
view model =
    div [] 
    (
        List.map viewFromTagManager model.tagManagerList
        ++
        List.map (\tagText -> p [] [text tagText]) (getSelectedTags model)
        ++
        [
            button [onClick AddTagManager] [text "Add new tag group"]
        ]
    )


viewFromTagManager : TagListManagerContainer -> Html Msg
viewFromTagManager {id, manager, enable} =
    let
        toggleMsg = if enable == False then 
                "Enable group"
            else 
                "Disable group"

        additionalClasses = 
            if enable then
                []
            else
                [Style.DisabledTag]

        toggleButton = button [onClick (ToggleListManager id)] [text toggleMsg]

        removeButton = a [Style.class [Style.RemoveButton], onClick (RemoveListManager id)] 
            [
                text "⊘"
            ]


        --nextButton = button [onClick ]
    in
        div [Style.class ([Style.TagListContainer] ++ additionalClasses)]
        [
            Html.App.map (TagListMsg id) (TagListManager.view manager),
            div []
            [
                toggleButton, 
                removeButton
            ]
        ]






-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none



-- Main
--main =
--    Html.App.program
--        {
--            init = init,
--            update = update,
--            view = view,
--            subscriptions = subscriptions
--        }
