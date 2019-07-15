module AlbumCommon exposing
    (tagEditorUrl)


tagEditorUrl : Int -> Int -> String
tagEditorUrl listId fileId =
    "tag_editor.html#list/"
              ++ (String.fromInt listId)
              ++ "/file/"
              ++ (String.fromInt fileId)

