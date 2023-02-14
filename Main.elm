module Main exposing (..)
import Browser
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Random
import Json.Decode exposing (..)
import Json.Decode.Pipeline as JP

getDefinition : String -> Cmd Msg
getDefinition answer = Http.get
    { url = "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ answer
    , expect = Http.expectJson GotDefinition (Json.Decode.list wordDecoder)
    }

type alias Definition =
    { definition : String
    , synonyms : List String
    , antonyms : List String
    }

type alias Meaning =
    { partsOfSpeech : String
    , definitions : List Definition
    }

type alias Word =
    { word : String
    , meanings : List Meaning
    }

defDecoder =
    succeed Definition
        |> JP.required "definition" string
        |> JP.required "synonyms" (Json.Decode.list string)
        |> JP.required "antonyms" (Json.Decode.list string)

meaningDecoder =
    succeed Meaning
        |> JP.required "partOfSpeech" string
        |> JP.required "definitions" (Json.Decode.list defDecoder)


wordDecoder =
    succeed Word
        |> JP.required "word" string
        |> JP.required "meanings" (Json.Decode.list meaningDecoder)

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- INIT

init : () -> (Model, Cmd Msg)
init _ =
    ( Loading
    , Http.get
        { url = "../static/Words.txt"
        , expect = Http.expectString ChooseWord
        }
    )

-- UPDATE

type Msg
    = Again
    | ChooseWord (Result Http.Error String)
    | RandomInt Int
    | GotDefinition (Result Http.Error (List Word))
    | NewGuess (List Word) String
    | ShowAnswer (List Word)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        -- Press on "Play again"
        Again -> (Loading, Navigation.reload)

        -- Take in the list of words and randomize an integer
        ChooseWord result ->
            case result of
                Ok wordList -> (GotList wordList, Random.generate RandomInt (Random.int 0 998))
                Err _ -> (Error "Failed to get the word list.", Cmd.none)

        -- Take in the word correspond to an integer chosen
        RandomInt n ->
            case model of
                GotList wordList ->
                    case String.split " " wordList of
                        [] -> (Error "The word list is empty.", Cmd.none)
                        (x::xs) -> case (List.head (List.drop n (x::xs))) of
                            Just answer -> (Loading, getDefinition answer)
                            Nothing -> (Error "Failed to pick a random word.", Cmd.none)
                Error err -> (Error err, Cmd.none)
                Loading -> (Loading, Cmd.none)
                Success _ _ -> (Error "", Cmd.none)
                Answer _ -> (Error "", Cmd.none)

        -- Take in the first word
        GotDefinition result ->
            case result of
                Ok wordList -> (Success wordList "", Cmd.none)
                Err _ -> (Error "Unable to retreive the json.", Cmd.none)
        
        -- Update the value of the answer given
        NewGuess wordList guess ->
            (Success wordList guess, Cmd.none)
  
        -- Show answer
        ShowAnswer wordList ->
            (Answer wordList, Cmd.none)
        
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- FUNCTIONS 

-- Synonyms
syn : Definition -> String
syn def = case def.synonyms of
    (x::xs) -> "\n-> synonyms: " ++ (String.join ", " def.synonyms)
    [] -> ""

-- Antonyms
ant : Definition -> String
ant def = case def.antonyms of
    (x::xs) -> "\n-> antonyms: " ++ (String.join ", " def.antonyms)
    [] -> ""

-- Return html message and each definition
toHtmlDef : Definition -> Html msg
toHtmlDef def = li [] (List.intersperse (br [] []) (List.map text
        (String.lines (def.definition ++ syn def ++ ant def))
    ))

-- Return lists of html definitions
defHtml : List Definition -> List (Html msg)
defHtml defList = case defList of
    def :: defs -> toHtmlDef def :: defHtml defs
    [] -> []

-- Return list of html with meaning correspond
meaningHtml : List Meaning -> List (Html msg)
meaningHtml meaningList = case meaningList of
    meaning :: meanings -> li [] [ text meaning.partsOfSpeech, ol [] (defHtml meaning.definitions) ] :: meaningHtml meanings 
    [] -> []

-- Return list of html with Json
wordHtml : List Word -> List (Html msg)
wordHtml wordList = case wordList of
    word :: words -> li [] [ ul [] (meaningHtml word.meanings) ] :: wordHtml words
    [] -> []


-- VIEW

type Model
  = Loading
  | Error String
  | GotList String
  | Success (List Word) String
  | Answer (List Word)

view model =
  case model of

    Loading ->
        div []
            [ h1 [ style "text-align" "center" , style "font-size" "50px" ] [ text "Guess it!" ]
            , div [ style "text-align" "center", style "font-size" "20px" ] [text "Loading..."]
            ]

    Error err ->
        div []
            [ b [ style "text-align" "center" ] [ text "ERROR" ]
            , div [] [ text err ]
            ]

    GotList _ ->
        text ""
    
    Success wordList guess -> case List.head wordList of
        Just word ->
            -- Waiting for answer from player
            if word.word /= guess then
                div []
                [ h1 [ style "text-align" "center", style "font-size" "50px" ] [ text "Guess it!" ]
                , div [ style "text-align" "center" ] [
                    input
                        [ style "text-align" "center"
                        , style "font-size" "20px"
                        , style "width" "193px"
                        , placeholder "Write your guess"
                        , Html.Attributes.value guess, onInput (NewGuess wordList) 
                        ] []
                    , br [] []
                    , button
                        [ style "text-align" "center"
                        , style "font-size" "20px"
                        , style "margin-top" "3px"
                        , style "width" "200px"
                        , onClick (ShowAnswer wordList) 
                        ] [ text "Show answer" ]
                    ]
                    , ol [ style "margin-left" "30px", style "margin-right" "100px" ] (wordHtml wordList)
                ]
            -- After giving answer, the player decide to play again or not + show the answer
            else
                div []
                [ h1 [ style "text-align" "center", style "font-size" "50px" ] [ text "Guess it!" ]
                , div [ style "text-align" "center" ]
                    [ div [ style "font-size" "20px" ] [ text ("The answer was '" ++ word.word ++ "'") ]
                    , button
                        [ style "text-align" "center"
                        , style "font-size" "20px"
                        , style "margin-top" "5px"
                        , style "width" "200px"
                        , onClick Again 
                        ] [ text "Play again" ]
                    , div [ style "margin-top" "90px", style "font-size" "70px" ] [ text "Congratulations!" ]
                    ]
                ]
        Nothing -> div []
            [ b [ style "text-align" "center" ] [ text "ERROR" ]
            , div [] [ text "The word list is empty." ]
            ]

    -- Show answer
    Answer wordList -> case List.head wordList of
        Just word ->
            div []
            [ h1 [ style "text-align" "center", style "font-size" "50px" ] [ text "Guess it!" ]
            , div [ style "text-align" "center" ]
                [ div [ style "font-size" "20px" ] [ text ("The answer was '" ++ word.word ++ "'") ]
                , button
                    [ style "text-align" "center"
                    , style "font-size" "20px"
                    , style "margin-top" "5px"
                    , style "width" "200px"
                    , onClick Again ] [ text "Play again" ]
                ]
            , ol [ style "margin-left" "30px", style "margin-right" "100px" ] (wordHtml wordList)
            ]
        Nothing -> div []
            [ b [ style "text-align" "center" ] [ text "ERROR" ]
            , div [] [ text "The word list is empty." ]
            ]