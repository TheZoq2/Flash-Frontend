module TagViews exposing
    ( tagListListHtml
    )

import Tags exposing
    ( TagListList
    , Tag
    , TagList
    , SelectedTag(..)
    , TagListMessages
    )

import Elements exposing (flatButton)

import Style
import Html exposing (..)
import Html.Attributes exposing (autofocus)
import Html.Events exposing (..)
import Dict


--Generates the html to display a single tag
htmlFromTag : Tag -> msg -> msg -> Html msg
htmlFromTag tag onTextClick onRemoveButton =
    let
        textClasses = case tag.enabled of
            True ->
                []
            False ->
                [Style.DisabledTag]
    in
        span [Style.class [Style.Tag]]
            [ span [onClick (onTextClick), Style.class textClasses] [text tag.text]
            , flatButton [Style.InlineButton] [] (onRemoveButton) "×" 1.5
            ]



htmlFromListOfTags : List (Style.CssClasses) -> List (Tag, msg, msg) -> Html msg
htmlFromListOfTags additionalClasses list =
    ul [Style.class additionalClasses]
        <| List.map 
            (\(tag, onTextClick, onRemoveButton) -> li [] [htmlFromTag tag onTextClick onRemoveButton]) 
            list






-- Returns the html for a given tag

tagListHtml : TagList -> (Int -> msg) -> (Int -> msg) -> Html msg
tagListHtml list onTextClick onRemoveButton =
    Dict.foldl 
        (\key value acc -> acc ++ [(value, (onTextClick key), (onRemoveButton key))])
        []
        list.tags
    |> (htmlFromListOfTags <| if not list.enabled then [Style.DisabledTag] else [])






-- Tag list list things


tagListButtonRow : TagList -> Int -> TagListMessages msg -> Maybe Int -> Html msg
tagListButtonRow tagList id messages textFieldTargetId =
    let
        toggleCharacter =
            case tagList.enabled of
                True -> "☑"
                False -> "☐"
        removeCharacter =
            "×"
        addCharacter =
            "+"

        buttonSize = 1.5

        toggleButton =
            flatButton 
                [Style.InlineButton]
                []
                (messages.onToggleList id)
                toggleCharacter
                buttonSize

        removeButton =
            flatButton
                [Style.InlineButton]
                []
                (messages.onRemoveList id)
                removeCharacter
                buttonSize

        addElement = case textFieldTargetId of
            Nothing ->
                flatButton 
                    [Style.InlineButton, Style.AddTagButton]
                    []
                    (messages.onAddTag id)
                    addCharacter
                    buttonSize

            Just(targetId) ->
                if targetId /= id then
                    flatButton 
                        [Style.InlineButton, Style.AddTagButton]
                        []
                        (messages.onAddTag id)
                        addCharacter
                        buttonSize
                else
                    form [Style.class [Style.AddTagButton], onSubmit <| messages.onTagSubmit id]
                        [input 
                            [Html.Attributes.id "tag_input_field"
                            , onBlur messages.onTagnameUnfocus
                            , onInput messages.onTextChanged
                            , Style.class [Style.TagTextField]
                            ] []]
    in
        div [Style.class [Style.TagListButtonRow]]
            [ toggleButton
            , addElement
            , removeButton
            ]

tagListListHtml : TagListList -> SelectedTag -> TagListMessages msg -> Html msg
tagListListHtml tagListList selectedTag messages =
    let
        foldFunction =
            (\id value acc -> 
                acc ++ [(id, (value, (messages.onTagTextClick id), (messages.onTagRemoveButton id)))])

        tagLists =
            Dict.foldl foldFunction [] tagListList.tagLists

        listClasses listId tagList =
            case selectedTag of
                None ->
                    []
                Single _ _ ->
                    []
                List id ->
                    if id == listId then [Style.TagEditorSelected] else []
            ++
            case tagList.enabled of
                True -> []
                False -> [Style.DisabledTag]

    in
        ul []
            <| List.map 
                (\(id, (tag, onText, onRemove)) ->
                    div [Style.class ([Style.TagList] ++ (listClasses id tag))]
                        [ tagListHtml tag onText onRemove
                        , tagListButtonRow tag id messages tagListList.textFieldTargetId
                        ]
                    )
                tagLists
