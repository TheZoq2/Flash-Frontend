module AlbumCommon exposing
    (tagEditorUrl)


tagEditorUrl : Int -> Int -> String
tagEditorUrl listId fileId =
    "tag_editor.html#list/"
              ++ (toString listId)
              ++ "/file/"
              ++ (toString fileId)
