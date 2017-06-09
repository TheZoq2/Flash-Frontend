module FileList exposing
    ( FileList
    , new
    , jump
    )

type alias FileList =
    { listId: Int
    , fileIndex: Int
    , length: Int
    }

new : Int -> Int -> FileList
new id length =
    FileList id 0 length



-- Attempts to set the index of the list to the current index + jump
-- If this the new index is outside of 0 < index < length, the original
-- list is returned
jump : Int -> FileList -> FileList
jump amount list =
    let
        newIndex = list.fileIndex + amount
    in
        if newIndex < list.length && newIndex >= 0 then
            { list | fileIndex = newIndex }
        else
            list
