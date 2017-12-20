module MsgCommand exposing (topLevel)

import CommandLine exposing (..)
import EditorMsg exposing (..)

topLevel : List String -> Command Msg
topLevel tags =
    NonTerminal Word ["hideUi", "addTag", "addTagList", "removeList", "toggleList"]
        (\query ->
            case query of
                "hideUi" -> Just <| Terminal EditorMsg.ToggleSidebar
                "addTagList" -> Just <| Terminal AddTagList
                "addTag" -> Just addTagCommand
                "removeList" -> singleIntMsg RemoveTagList
                "toggleList" -> singleIntMsg ToggleTagList
                _ -> Nothing
        )


intParam : (Int -> Command Msg) -> Command Msg
intParam restProducer =
    NonTerminal Word []
        (\query ->
            Result.toMaybe (String.toInt query)
            |> Maybe.map restProducer
        )

tagCommand : List String -> (String -> Msg) -> Command Msg
tagCommand tags msg =
    NonTerminal Rest tags
        (\query ->
            Just <| Terminal <| msg query
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

