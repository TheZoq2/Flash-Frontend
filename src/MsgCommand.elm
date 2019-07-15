module MsgCommand exposing (topLevel)

import CommandLine exposing (..)
import EditorMsg exposing (..)

topLevel : List String -> Command Msg
topLevel tags =
    NonTerminal Word ["hideUi", "addTag", "addTagList", "removeList", "toggleList", "toggleTag", "removeTag"]
        (\query ->
            case query of
                "hideUi" -> Just <| Terminal EditorMsg.ToggleSidebar
                "addTagList" -> Just <| Terminal AddTagList
                "addTag" -> Just addTagCommand
                "removeList" -> singleIntMsg RemoveTagList
                "toggleList" -> singleIntMsg ToggleTagList
                "removeTag" -> Just <| singleStringCommand RemoveTagByName tags
                "toggleTag" -> Just <| singleStringCommand ToggleTagByName tags
                _ -> Nothing
        )


intParam : (Int -> Command Msg) -> Command Msg
intParam restProducer =
    NonTerminal Word []
        (\query ->
            (String.toInt query)
            |> Maybe.map restProducer
        )

addTagCommand : Command Msg
addTagCommand =
    intParam
        (\groupId ->
            NonTerminal Rest []
                (\query ->
                    Just <| Terminal <| AddTag groupId query
                )
        )

singleIntMsg : (Int -> Msg) -> Maybe (Command Msg)
singleIntMsg msg =
    Just <| intParam (\groupId -> Terminal <| msg groupId)


singleStringCommand : (String -> Msg) -> List String -> Command Msg
singleStringCommand msg suggestions =
    NonTerminal Rest suggestions
        (\query ->
            Just <| Terminal <| msg query
        )
