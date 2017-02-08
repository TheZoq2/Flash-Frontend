module TagListList exposing (Model, Msg, init, update, getSelectedTags, view, handleKeyboardInput, setOldTags)

import TagListManager
import Style
import Html exposing (..)
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
    { id : Int
    , manager : TagListManager.Model
    , enable : Bool
    , focusKey : Char
    }


initContainer : Int -> Char -> TagListManagerContainer
initContainer id focusKey =
    { id = id
    , manager = TagListManager.init
    , enable = True
    , focusKey = focusKey
    }


initContainerWithTags : List String -> Int -> Char -> TagListManagerContainer
initContainerWithTags tags id focusKey =
    { id = id
    , manager = TagListManager.initWithTagNames tags
    , enable = True
    , focusKey = focusKey
    }


focusKeys =
    [ 'J', 'K', 'L', 'A', 'S', 'D', 'F', 'G', 'H', 'Q', 'W', 'E', 'R', 'Z', 'X', 'C', 'V', 'B', 'N', 'M' ]


type KeyState
    = Global
    | Remove
    | Disable
    | Select
    | Selected Int


type alias Model =
    { tagManagerList : List TagListManagerContainer
    , existingTagListId : Maybe Int
    , --The ID of the tag list that contains the tags already contained
      --in the image
      nextTagListId : Int
    , focusKeyList : List Char
    , keyboardState : KeyState
    }


init : Model
init =
    Model [] Nothing 0 focusKeys Global



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
            { model | tagManagerList = List.map (tagManagerUpdateHelper id tagListMsg) model.tagManagerList }

        AddTagManager ->
            addTagListManager model

        ToggleListManager id ->
            toggleTagListManager model id

        RemoveListManager id ->
            removeTagListManager model id

        KeyPressed code ->
            handleKeyboardInput model code



-- UPDATE HELPER FUNCTIONS


handleKeyboardInput : Model -> Int -> Model
handleKeyboardInput model code =
    let
        runFunctionOnSelected model func char =
            case getManagerWithFocusKey model.tagManagerList <| char of
                Just id ->
                    let
                        newModel =
                            (func model id)
                    in
                        { newModel | keyboardState = Global }

                Nothing ->
                    model
    in
        case model.keyboardState of
            Global ->
                case Char.fromCode code of
                    'A' ->
                        addTagListManager model

                    'R' ->
                        { model | keyboardState = Remove }

                    'D' ->
                        { model | keyboardState = Disable }

                    _ ->
                        model

            Remove ->
                runFunctionOnSelected model removeTagListManager <| Char.fromCode code

            Disable ->
                runFunctionOnSelected model toggleTagListManager <| Char.fromCode code

            Select ->
                case getManagerWithFocusKey model.tagManagerList <| Char.fromCode code of
                    Just id ->
                        { model | keyboardState = Selected id }

                    Nothing ->
                        model

            _ ->
                model


removeTagListManager : Model -> Int -> Model
removeTagListManager model id =
    let
        ( newContainerList, removed ) =
            List.partition (\manager -> manager.id /= id) model.tagManagerList

        removedFocusKeyList =
            List.map (.focusKey) removed
    in
        { model
            | tagManagerList = newContainerList
            , keyboardState = Global
            , focusKeyList = removedFocusKeyList ++ model.focusKeyList
            , keyboardState = Global
        }


toggleTagListManager : Model -> Int -> Model
toggleTagListManager model id =
    let
        toggleWithId manager =
            if manager.id == id then
                { manager | enable = manager.enable == False }
            else
                manager

        newContainerList =
            List.map toggleWithId model.tagManagerList
    in
        { model | tagManagerList = newContainerList }


addTagListManager : Model -> Model
addTagListManager model =
    addTagListManagerWithTags model []


addTagListManagerWithTags : Model -> List String -> Model
addTagListManagerWithTags model tags =
    let
        --Dealing with the fact that the list can be empty. TODO: Generate a new list
        nextFocusKey =
            case List.head model.focusKeyList of
                Just val ->
                    val

                Nothing ->
                    '\\'

        nextFocusList =
            case List.tail model.focusKeyList of
                Just list ->
                    list

                Nothing ->
                    []

        newContainerList =
            model.tagManagerList
                ++ [ initContainerWithTags tags model.nextTagListId <| nextFocusKey ]

        nextTagListId =
            model.nextTagListId + 1
    in
        { model
            | tagManagerList = newContainerList
            , nextTagListId = nextTagListId
            , focusKeyList = nextFocusList
        }


tagManagerUpdateHelper : Int -> TagListManager.Msg -> TagListManagerContainer -> TagListManagerContainer
tagManagerUpdateHelper targetId msg container =
    let
        manager =
            container.manager
    in
        { container
            | manager =
                (if targetId == container.id then
                    TagListManager.update msg manager
                 else
                    manager
                )
        }


getManagerWithFocusKey : List TagListManagerContainer -> Char -> Maybe Int
getManagerWithFocusKey list key =
    case list of
        [] ->
            Nothing

        _ ->
            let
                elem =
                    Maybe.withDefault (initContainer 0 'a') <| List.head list

                rest =
                    Maybe.withDefault [] <| List.tail list
            in
                if elem.focusKey == key then
                    Just elem.id
                else
                    getManagerWithFocusKey rest key


getSelectedTags : Model -> List String
getSelectedTags model =
    let
        enabledTagContainers =
            List.filter (\manager -> manager.enable == True) model.tagManagerList

        managers =
            List.map .manager enabledTagContainers
    in
        List.map TagListManager.getSelectedTags managers |> List.concat


setOldTags : Model -> List String -> Model
setOldTags model tags =
    let
        model_ =
            case model.existingTagListId of
                Nothing ->
                    model

                Just id ->
                    removeTagListManager model id
    in
        case tags of
            [] ->
                let
                    _ =
                        Debug.log "No previous tags" ""
                in
                    { model_ | existingTagListId = Nothing }

            list ->
                let
                    currentTags =
                        List.map String.toLower <| getSelectedTags model_

                    shownTags =
                        List.filter (\elem -> List.member elem currentTags == False) tags

                    model__ =
                        addTagListManagerWithTags model_ shownTags
                in
                    { model_ | existingTagListId = Just <| model__.nextTagListId - 1 }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        (List.map viewFromTagManager model.tagManagerList
            ++ List.map (\tagText -> p [] [ text tagText ]) (getSelectedTags model)
            ++ [ button [ onClick AddTagManager ] [ text "Add new tag group" ]
               ]
        )


viewFromTagManager : TagListManagerContainer -> Html Msg
viewFromTagManager { id, manager, enable, focusKey } =
    let
        toggleMsg =
            if enable == False then
                "Enable group"
            else
                "Disable group"

        additionalClasses =
            if enable then
                []
            else
                [ Style.DisabledTag ]

        toggleButton =
            button [ onClick (ToggleListManager id) ] [ text toggleMsg ]

        removeButton =
            a [ Style.class [ Style.RemoveButton ], onClick (RemoveListManager id) ]
                [ text "⊘"
                ]
    in
        div [ Style.class ([ Style.TagListContainer ] ++ additionalClasses) ]
            [ Html.map (TagListMsg id) (TagListManager.view manager)
            , div []
                [ --p [] [text <| String.fromChar focusKey],
                  toggleButton
                , removeButton
                ]
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
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
