module TagListManager exposing (Tag)

import Html exposing (..)

type alias Tag =
    { text : String
    , selected : Bool
    }

selectedTags : List Tag -> List String
selectedTags tags =
    List.map (\x -> x.text) <| List.filter (\x -> x.selected) tags


htmlFromTag : Tag -> a -> a -> Html a
htmlFromTag tag onTextClick onRemoveButton =
    p [] [text tag.text]

