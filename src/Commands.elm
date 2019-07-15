module Commands exposing 
    ( CommandData
    , initCommandData
    , CommandLineEvents
    , commandLineView
    )

import Html.Styled exposing (..)
import Html.Styled.Events exposing (onBlur, onInput)
import Html.Styled.Attributes exposing (css)

import CommandLine
import Style


{-|
  Data for the current fuzzy command input
-}
type alias CommandData =
    { query: String
    , expandedQuery: String
    , suggestions: List (String, List Bool)
    }

initCommandData : CommandLine.Command a -> Result CommandLine.FuzzyError CommandData
initCommandData command =
    case CommandLine.expandCommand "" command of
        (expansion, Ok suggestions) ->
            Ok
                { query = ""
                , expandedQuery = expansion
                , suggestions = suggestions
                }
        (expansion, Err e) ->
            Err e


{-|
  Event handlers for various commandline events
-}
type alias CommandLineEvents a =
    { onBlur: a
    , onInput: String -> a
    }

{-|
  Renders an input field, the expansion of the current query along with
  the suggestions for the current word.
-}
commandLineView : CommandLineEvents a -> CommandData -> Html a
commandLineView events data =
    div
        [css [Style.commandLineContainerStyle]]
        [ input
            [ onInput events.onInput
            -- onBlur events.onBlur
            , Html.Styled.Attributes.id "command_field"
            ]
            []
        , div [] [p [] [text data.expandedQuery]]
        , div [] [ul [] <| List.map renderSuggestion data.suggestions]
        ]

{-|
  Renders a single fuzzy suggestion by rendering matches in bold
-}
renderSuggestion : (String, List Bool) -> Html a
renderSuggestion (string, matches) =
    List.map2 Tuple.pair (String.toList string) matches
    |> List.map (\(char, match) ->
        (if match then strong [] else span []) <| [text <| String.fromChar char])
    |> li []




