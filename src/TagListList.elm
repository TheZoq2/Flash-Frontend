module TagListList exposing(Model, Msg, init, update, getSelectedTags, view, handleKeyboardInput)

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
import Char
import String


-- MODEL

type alias TagListManagerContainer =
    {
        id: Int,
        manager: TagListManager.Model,
        enable: Bool,
        focusKey: Char
    }
initContainer : Int -> Char -> TagListManagerContainer
initContainer id focusKey =
    {
        id = id,
        manager = TagListManager.init,
        enable = True,
        focusKey = focusKey
    }

focusKeys = 
    ['J', 'K', 'L', 'A', 'S', 'D', 'F', 'G', 'H', 'Q', 'W', 'E', 'R', 'Z', 'X', 'C', 'V', 'B', 'N', 'M']



type alias Model =
    {
        tagManagerList: List TagListManagerContainer,
        nextTagListId: Int,
        focusKeyList: List Char
    }


init : Model
init =
    Model [] 0 focusKeys





-- UPDATE

type Msg
    = TagListMsg Int TagListManager.Msg
    | AddTagManager
    | ToggleListManager Int
    | RemoveListManager Int
    | KeyPressed Int


update : Msg -> Model -> Model
update msg model =
    case msg of 
        TagListMsg id tagListMsg ->
            {model | tagManagerList = List.map (tagManagerUpdateHelper id tagListMsg) model.tagManagerList}

        AddTagManager ->
            addTagListManager model
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

        KeyPressed code ->
            handleKeyboardInput model code




-- UPDATE HELPER FUNCTIONS

handleKeyboardInput : Model -> Int -> Model
handleKeyboardInput model code =
    case Char.fromCode code of
        'A' -> 
            addTagListManager model
        _ ->
            model


addTagListManager : Model -> Model
addTagListManager model =
    let
        --Dealing with the fact that the list can be empty. TODO: Generate a new list
        nextFocusKey = case List.head model.focusKeyList of
            Just val ->
                val
            Nothing ->
                '\\'
        nextFocusList = case List.tail model.focusKeyList of
            Just list -> 
                list
            Nothing ->
                []

        newContainerList = model.tagManagerList ++ 
            [initContainer model.nextTagListId <| nextFocusKey]

        nextTagListId = model.nextTagListId + 1

    in
        {model | 
            tagManagerList = newContainerList,
            nextTagListId = nextTagListId,
            focusKeyList = nextFocusList}


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
viewFromTagManager {id, manager, enable, focusKey} =
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
                p [] [text <| String.fromChar focusKey],
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
