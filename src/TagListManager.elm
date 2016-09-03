module TagListManager exposing (Model, Msg, init, update, view, getTagList, getSelectedTags)

import Style

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode 


--Model
type alias Model =
    {
        tags: List Tag,
        nextId: Int,

        currentTextFieldText: String
    }

type alias Tag =
    {
        id: Int,
        name: String,
        enable: Bool
    }

init : Model
init = 
    Model [] 0 ""




--Returns a list of the name of all the tags in the model
getTagList : Model -> List String
getTagList model =
    List.map (\tag -> tag.name) model.tags







--Update function

type Msg
    = AddTag
    | RemoveTag Int
    | InputChanged String
    | ToggleTag Int


update: Msg -> Model -> Model
update msg model =
    case msg of 
        AddTag ->
            {model | 
                tags = model.tags ++ [(Tag model.nextId model.currentTextFieldText True)],
                nextId = model.nextId + 1,
                currentTextFieldText = ""
            }

        RemoveTag index ->
            {model | tags = List.filter (\tag -> tag.id /= index) model.tags}
        
        InputChanged text ->
            {model | currentTextFieldText=text}


        ToggleTag id ->
            let
                toggleWithId tag =
                    if tag.id == id then
                        --Bool == False is the same as !Bool
                        {tag | enable = tag.enable == False}
                    else
                        tag

            in
                {model | tags = List.map toggleWithId model.tags}






--View rendering

view : Model -> Html Msg
view model =
    div []
    [
        Html.form [onSubmit AddTag 
            ]
        [
            input [placeholder "Add tag", onInput InputChanged, value model.currentTextFieldText] []
        ],
        ul [] (List.map viewTag model.tags)
    ]


--Create the HTML code for a single tag
viewTag: Tag -> Html Msg
viewTag tag =
    let 
        toggleState = if tag.enable == True then "enabled" else "disabled"

        classList = if tag.enable == True then [] else [Style.DisabledTag]
    in
    li [Style.class [Style.TagListLi]]
    [
        p [Style.class ([Style.TagListName] ++ classList), onClick (ToggleTag tag.id) ] [text tag.name],

        --button [onClick (RemoveTag tag.id)] [text "⊘"]
        p [Style.class [Style.RemoveButton], onClick (RemoveTag tag.id)] [text "⊘"]
    ]



getSelectedTags : Model -> List String
getSelectedTags model =
    let
        isEnabled tag = 
            tag.enable == True
    in
        List.filter isEnabled model.tags |> List.map .name



main =
  Html.App.beginnerProgram
    { model = init
    , update = update
    , view = view
    }

