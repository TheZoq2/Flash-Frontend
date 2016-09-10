module TagEditor exposing(..)

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
        nextTagListId: Int,
        currentImage: String,
        currentImageDimensions: (Int, Int),
        lastError: String
    }

init: (Model, Cmd Msg)
init = 
    (Model [] 0 "" (0,0) "", requestNewImage Current)





-- UPDATE

type Msg
    = TagListMsg Int TagListManager.Msg
    | AddTagManager
    | ToggleListManager Int
    | RemoveListManager Int
    | RequestNext
    | RequestPrev
    | RequestCurrent
    | RequestSave
    | NetworkError Http.Error
    | NewImageReceived ImageResponse
    | OnSaved String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        TagListMsg id tagListMsg ->
            ({model | tagManagerList = List.map (tagManagerUpdateHelper id tagListMsg) model.tagManagerList}, Cmd.none)

        AddTagManager ->
            let
                newContainerList = model.tagManagerList ++ [initContainer model.nextTagListId]

                nextTagListId = model.nextTagListId + 1
            in
                ({model | tagManagerList = newContainerList, nextTagListId = nextTagListId}, Cmd.none)

        ToggleListManager id ->
            let
                toggleWithId manager = 
                    if manager.id == id then
                        {manager | enable = manager.enable == False}
                    else
                        manager

                newContainerList = List.map toggleWithId model.tagManagerList

            in
                ({model | tagManagerList = newContainerList}, Cmd.none)

        RemoveListManager id ->
            let
                newContainerList = List.filter (\manager -> manager.id /= id) model.tagManagerList
            in
                ({model | tagManagerList = newContainerList }, Cmd.none)

        RequestNext ->
            (model, requestNewImage Next)
        RequestPrev ->
            (model, requestNewImage Prev)
        RequestCurrent ->
            (model, requestNewImage Current)

        RequestSave ->
            (model, requestSaveImage (getSelectedTags model))

        OnSaved _ ->
            (model, requestNewImage Next)


        NetworkError e ->
            let
                _ = Debug.log "Network error" e
            in
                (model, Cmd.none)

        NewImageReceived response ->
            ({model | 
                currentImage = "http://localhost:3000/" ++ response.filePath,
                currentImageDimensions = response.dimensions
            } , Cmd.none)


type ImageDirection
    = Next
    | Prev
    | Current

requestNewImage : ImageDirection -> Cmd Msg
requestNewImage direction = 
    let 
        action = case direction of 
            Next ->
                "next"
            Prev ->
                "prev"
            Current ->
                "current"

        url = "http://localhost:3000/list?action=" ++ action
    in
        Task.perform NetworkError NewImageReceived (Http.get decodeNewImage url)

requestSaveImage : (List String) -> Cmd Msg
requestSaveImage tags =
    let 
        --Encode the tag list
        tagsJson = List.map Json.Encode.string tags

        --TODO: Make surre X-Origin requests are allowed
        url = "http://localhost:3000/list?action=save&tags=" ++ toString tagsJson
    in
        Task.perform NetworkError OnSaved (Http.get (Json.Decode.at [] Json.Decode.string) url)



type alias ImageResponse = {
        filePath: String,
        dimensions: (Int, Int)
    }
decodeNewImage : Json.Decode.Decoder ImageResponse
decodeNewImage =
    let
        decodeDimensions =
            Json.Decode.tuple2 (,) Json.Decode.int Json.Decode.int

        decodeMsg = 
            Json.Decode.object2 ImageResponse 
                ("file_path" := Json.Decode.string) 
                ("dimensions" := decodeDimensions)
    in
        decodeMsg


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
    let
        nextButton = button [onClick RequestNext] [text "Next"]
        prevButton = button [onClick RequestPrev] [text "Prev"]
        saveButton = button [onClick RequestSave] [text "Save"]

        buttonRow = div [Style.class [Style.TagEditorButtonRow]] [prevButton, nextButton, saveButton]
    in
    div [Style.class [Style.TagEditorContainer]]
    [
        div [Style.class [Style.TagEditorContentContainer]]
        [
            img [
                    src model.currentImage
                ] []
        ],
        div [Style.class [Style.TagEditorRightPane]] 
        (
            [
                buttonRow
            ]
            ++
            List.map viewFromTagManager model.tagManagerList
            ++
            List.map (\tagText -> p [] [text tagText]) (getSelectedTags model)
            ++
            [
                button [onClick AddTagManager] [text "Add new tag group"]
            ]
        )
    ]


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
main =
    Html.App.program
        {
            init = init,
            update = update,
            view = view,
            subscriptions = subscriptions
        }
