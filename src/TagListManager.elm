module TagListManager exposing (Tag, addTag, selectedTagsText, htmlFromTagList)

import Html exposing (..)
import Html.Events exposing (..)

import Elements exposing (flatButton)
import Style

type alias Tag =
    { id: Int
    , text: String
    , selected: Bool
    }



--Adds a tag to a list of existing tags. The new tag is given the specified
--id and the new list aswell as a new id is returned
addTag : Int -> List Tag -> String -> (List Tag, Int)
addTag id tags tagText =
    let
        tag =
            Tag id tagText True
    in
        (tags ++ [tag], id + 1)





--Returns a list of the text of all selected tags in a list of tags
selectedTagsText : List Tag -> List String
selectedTagsText tags =
    List.map (\x -> x.text) <| List.filter (\x -> x.selected) tags





--Generates the html to display a single tag
htmlFromTag : Tag -> (Int -> msg) -> (Int -> msg) -> Html msg
htmlFromTag tag onTextClick onRemoveButton =
    span []
        [ p [onClick (onTextClick tag.id)] [text tag.text]
        , flatButton [Style.InlineButton] [] (onRemoveButton tag.id) "x" 1
        ]



htmlFromTagList : List Tag -> (Int -> msg) -> (Int -> msg) -> Html msg
htmlFromTagList list onTextClick onRemoveButton=
    ul []
        <| List.map (\tag -> li [] [htmlFromTag tag onTextClick onRemoveButton]) list





-- TagLists

type alias TagList =
    { nextTagId
    , tags: List Tag
    , enabled: Bool
    }


-- Adds a single tag to a tag list

addTagList : List TagList -> Int -> (List TagList, Int)
addTagList oldList id =
    (List.append oldList <| TagList id [], id + 1)




-- Adds a tag with a specific text to a tag list in a list of tag lists

addTagToList : List TagList -> Int -> String -> TagList
addTagToList tagLists listId tag =
    let
        addFunction tagList =
            if tagList.id == listId then
                {tagList | tags = addTag tag}
            else
                tagList
    in
        List.map addFunction tagLists
