module Tags exposing 
    (Tag
    , TagList
    , TagListList
    , TagListMessages
    , SelectedTag(..)
    , emptyTagList
    , addTagToList
    , removeTag
    , tagListSelectedTags
    , selectedTags
    , addTagList
    , removeTagList
    , emptyTagListList
    , removeTagFromTagListList
    , tagListListHtml
    , addTagToTagListList
    , toggleTagList
    , toggleTagInTagListList
    , startTagTextInput
    , cancelAddTag
    , getNthTagListId
    )

import Html exposing (..)
import Html.Attributes exposing (autofocus)
import Html.Events exposing (..)

import Elements exposing (flatButton)
import Style
import Dict exposing (Dict)

import List.Extra

type alias Tag =
    { text: String
    , enabled: Bool
    }

newTag : String -> Tag
newTag text =
    Tag text True


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



htmlFromListOfTags : List (Tag, msg, msg) -> Html msg
htmlFromListOfTags list =
    ul []
        <| List.map 
            (\(tag, onTextClick, onRemoveButton) -> li [] [htmlFromTag tag onTextClick onRemoveButton]) 
            list





-- TagLists

type alias TagList =
    { nextId: Int
    , tags: Dict Int Tag
    , enabled: Bool
    }


emptyTagList : TagList
emptyTagList =
    TagList 0 Dict.empty True



-- Add a tag to a tag list

addTagToList : String -> TagList -> TagList
addTagToList tagText list=
    {list | tags = Dict.insert list.nextId (newTag tagText) list.tags, nextId = list.nextId + 1}


removeTag : Int -> TagList -> TagList
removeTag id list =
    {list | tags = Dict.remove id list.tags}



runOnTag : (Tag -> Tag) -> Int -> TagList -> TagList
runOnTag function targetId list=
    let
        mapFunction id tag =
            if targetId == id then
                function tag
            else
                tag
    in
        { list | tags = Dict.map mapFunction list.tags }


toggleTag : Int -> TagList -> TagList
toggleTag id list =
    runOnTag (\tag -> { tag | enabled = not tag.enabled }) id list



-- Returns a list of the text of all enabled tags in a tag list

tagListSelectedTags : TagList -> List String
tagListSelectedTags list =
    let
        tagList = Dict.foldl (\_ tag list -> list ++ [tag]) [] list.tags
    in
        List.filter (\tag -> tag.enabled) tagList
        |> List.map (\tag -> tag.text)



-- Returns the html for a given tag

tagListHtml : TagList -> (Int -> msg) -> (Int -> msg) -> Html msg
tagListHtml list onTextClick onRemoveButton =
    Dict.foldl 
        (\key value acc -> acc ++ [(value, (onTextClick key), (onRemoveButton key))])
        []
        list.tags
    |> htmlFromListOfTags





-- TagListLists
type alias TagListList =
    { nextId: Int
    , tagLists: Dict Int TagList
    , textFieldTargetId: Maybe Int
    }

emptyTagListList =
    TagListList 0 Dict.empty Nothing




--Adds a tag list to a tag list list

addTagList : TagList -> TagListList -> TagListList
addTagList addedList targetList =
    { targetList 
        | tagLists = Dict.insert targetList.nextId addedList targetList.tagLists
        , nextId = targetList.nextId + 1
        }

removeTagList : Int -> TagListList -> TagListList
removeTagList listId tagListList =
    let
        filterFunction key value =
            key /= listId
    in
        { tagListList | tagLists = Dict.filter filterFunction tagListList.tagLists }



startTagTextInput : Int -> TagListList -> TagListList
startTagTextInput id list =
    {list | textFieldTargetId = Just id}


cancelAddTag : TagListList -> TagListList
cancelAddTag list =
    {list | textFieldTargetId = Nothing}


-- Runs a function on a specified tag list

runOnTagList : (TagList -> TagList) -> Int -> TagListList -> TagListList
runOnTagList fn targetId tagListList =
    let
        mapFunction key value = 
            if key == targetId then
                fn value
            else
                value
    in
        { tagListList | tagLists = Dict.map mapFunction tagListList.tagLists }



-- Adds a tag to a tag list inside a tag list list

addTagToTagListList : String -> Int -> TagListList -> TagListList
addTagToTagListList tagText listId listList =
    runOnTagList (addTagToList tagText) listId listList




-- Removes the tag with tagId from the list with listId

removeTagFromTagListList : Int -> Int -> TagListList -> TagListList
removeTagFromTagListList listId tagId tagListList = 
    runOnTagList (removeTag tagId) listId tagListList

-- Toggles the tag with tagId from the list with listId

toggleTagInTagListList : Int -> Int -> TagListList -> TagListList
toggleTagInTagListList listId tagId tagListList = 
    runOnTagList (toggleTag tagId) listId tagListList


-- Togles the specified tag list
toggleTagList : Int -> TagListList -> TagListList
toggleTagList id list =
    runOnTagList (\list -> {list | enabled = not list.enabled}) id list


-- Returns a list of selected tags in a tagListList

selectedTags : TagListList -> List String
selectedTags list =
    let
        foldFunction _ value acc = 
            acc ++ if value.enabled then tagListSelectedTags value else []
    in
        Dict.foldl foldFunction [] list.tagLists



type alias TagListMessages msg =
    { onAddTag: (Int -> msg)
    , onRemoveList: (Int -> msg)
    , onToggleList: (Int -> msg)
    , onTagRemoveButton: (Int -> Int -> msg)
    , onTagTextClick: (Int -> Int -> msg)
    , onTagnameUnfocus: msg
    , onTagSubmit: (Int -> msg)
    , onTextChanged: (String -> msg)
    }

type SelectedTag
    = None
    | List Int
    | Single Int Int

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

        --TODO: Improve code quality in this function
        buildButtonRow : TagList -> Int -> Html msg
        buildButtonRow tagList id =
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

                addElement = case tagListList.textFieldTargetId of
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
    in
        ul []
            <| List.map 
                    (\(id, (tag, onText, onRemove)) -> 
                        div [Style.class ([Style.TagList] ++ (listClasses id tag))]
                            [ tagListHtml tag onText onRemove
                            , buildButtonRow tag id
                            ]
                        )
                    tagLists



getNthTagListId : TagListList -> Int -> Maybe Int
getNthTagListId tagListList target =
    List.Extra.getAt target <| Dict.keys tagListList.tagLists

