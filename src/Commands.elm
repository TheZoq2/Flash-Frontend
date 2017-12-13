module Commands

type alias CommandData =
    { query: String
    , expandedQuery: String
    , suggestions: List (String, List Bool)
    }


type alias CommandLineEvents a =
    { onBlur: a
    , onInput: String -> a
    , onSubmit: a
    }

commandLineView : CommandLineEvents -> CommandData -> Html a
commandLineView events data =
    div
        []
        [ div [] [input [onBlur events.onBlur, onInput events.onInput] []]
        ]
