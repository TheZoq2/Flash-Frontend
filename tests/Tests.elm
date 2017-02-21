module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String

import TagListManager


tagListTest : Test
tagListTest =
    describe "Tag list management"
        [describe "adding and removing tags"
            [ test "additionOnly" <|
                \() ->
                    let
                        manager 
                            =  TagListManager.emptyTagList
                            |> TagListManager.addTagToList "yolo"
                            |> TagListManager.addTagToList "swag"
                    in
                        Expect.equal ["yolo", "swag"] <| (TagListManager.selectedTags manager)
            , test "removal" <|
                \() ->
                    let
                        manager 
                            =  TagListManager.emptyTagList
                            |> TagListManager.addTagToList "yolo"
                            |> TagListManager.addTagToList "swag"
                            |> TagListManager.addTagToList "something"
                            |> TagListManager.removeTag 1
                    in
                        Expect.equal ["yolo", "something"] <| (TagListManager.selectedTags manager)
            ]
        ]


