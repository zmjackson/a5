module Main exposing (Message, Model, Msg(..), Status(..), init, main, update, view, viewCustomMessage, viewInput, viewMessage, viewMessages, viewSuggestions)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, br, button, div, footer, form, h1, input, label, li, option, p, select, text, ul)
import Html.Attributes exposing (action, class, selected, type_, value)
import Html.Events exposing (on, onClick, onInput, onSubmit, targetValue)
import Http exposing (header)
import Json.Decode exposing (Decoder, field, list, string)
import Json.Encode as Encode



--MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "A5"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias User =
    { name : String, language : String }


type CurrentUser
    = User1
    | User2


type alias Model =
    { user1 : User
    , user2 : User
    , currentUser : CurrentUser
    , messages : List Message
    , composeMessage : String
    , suggestions : List Message
    , translatedSuggestions : List Message
    }


s =
    [ Message User1 Understand "I understand"
    , Message User1 Understand "I am at my destination"
    , Message User1 Confused "Can you give more detail?"
    , Message User1 Confused "I do not see it"
    , Message User1 Confused "That does not make sense"
    ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { user1 = { name = "Zach", language = "en" }
      , user2 = { name = "Zhang", language = "zh-Hans" }
      , currentUser = User1
      , messages = []
      , composeMessage = ""
      , suggestions = s
      , translatedSuggestions = []
      }
    , getTranslation Suggestions s ( "en", "en" )
    )



-- UPDATE


type Status
    = Understand
    | Confused
    | Neutral


type WhichList
    = Suggestions
    | Messages


type alias Message =
    { sender : CurrentUser, status : Status, text : String }


type alias TranslationResult =
    List (List String)


type Msg
    = SwitchUser
    | ChangeLanguage String
    | SendMessage Message
    | GotTranslation WhichList (List Message) (Result Http.Error TranslationResult)
    | TypeMessage String


processTranslation : TranslationResult -> List String
processTranslation translations =
    case List.head translations of
        Just t ->
            t

        Nothing ->
            List.map (\_ -> "???") translations


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SwitchUser ->
            let
                u =
                    case model.currentUser of
                        User1 ->
                            ( User2, ( model.user1.language, model.user2.language ) )

                        User2 ->
                            ( User1, ( model.user2.language, model.user1.language ) )
            in
            ( { model
                | currentUser = Tuple.first u
              }
            , Cmd.batch
                [ getTranslation Suggestions model.suggestions ( "en", Tuple.second (Tuple.second u) )
                , getTranslation Messages model.messages (Tuple.second u)
                ]
            )

        ChangeLanguage newLanguage ->
            case model.currentUser of
                User1 ->
                    let
                        updated =
                            model.user1
                    in
                    ( { model | user1 = { updated | language = newLanguage } }
                    , Cmd.batch
                        [ getTranslation Suggestions model.suggestions ( "en", newLanguage )
                        , getTranslation Messages model.messages ( model.user1.language, newLanguage )
                        ]
                    )

                User2 ->
                    let
                        updated =
                            model.user2
                    in
                    ( { model | user2 = { updated | language = newLanguage } }
                    , Cmd.batch
                        [ getTranslation Suggestions model.suggestions ( "en", newLanguage )
                        , getTranslation Messages model.messages ( model.user2.language, newLanguage )
                        ]
                    )

        SendMessage message ->
            ( { model
                | composeMessage = ""
                , messages = List.append model.messages [ message ]
              }
            , Cmd.none
            )

        GotTranslation whichList messages result ->
            case result of
                Ok translations ->
                    let
                        newContent =
                            List.map2
                                (\m lt ->
                                    { m
                                        | text =
                                            case List.head lt of
                                                Just t ->
                                                    t

                                                Nothing ->
                                                    "???"
                                    }
                                )
                                messages
                                translations
                    in
                    case whichList of
                        Suggestions ->
                            ( { model
                                | translatedSuggestions = newContent
                              }
                            , Cmd.none
                            )

                        Messages ->
                            ( { model
                                | messages = newContent
                              }
                            , Cmd.none
                            )

                Err _ ->
                    ( model, Cmd.none )

        TypeMessage newInput ->
            ( { model | composeMessage = newInput }, Cmd.none )



-- VIEW


viewMessage : CurrentUser -> Message -> Html Msg
viewMessage current message =
    let
        messageClass =
            if message.sender == current then
                "mine"

            else
                "theirs"

        mesageStatus =
            case message.status of
                Understand ->
                    " chat-understand"

                Confused ->
                    " chat-confused"

                Neutral ->
                    " chat-neutral"
    in
    div [ class (messageClass ++ "-wrapper") ]
        [ p [ class ("message " ++ messageClass ++ mesageStatus) ] [ text message.text ]
        ]


viewMessages : CurrentUser -> String -> List Message -> Html Msg
viewMessages user name messages =
    div [ class "chat-area" ]
        [ h1 [ class "chat-name" ] [ text name ]
        , div [ class "messages" ] (List.map (viewMessage user) messages)
        ]


viewSuggestions : CurrentUser -> List Message -> Html Msg
viewSuggestions user suggestions =
    ul [ class "response-options" ]
        (List.map
            (\suggestion ->
                li
                    [ class
                        (if suggestion.status == Understand then
                            "suggestion understand"

                         else
                            "suggestion confused"
                        )
                    , onClick <|
                        SendMessage <|
                            Message
                                user
                                suggestion.status
                                suggestion.text
                    ]
                    [ text suggestion.text ]
            )
            suggestions
        )


viewCustomMessage : CurrentUser -> String -> Html Msg
viewCustomMessage user message =
    form [ onSubmit (SendMessage { sender = user, status = Neutral, text = message }) ]
        [ input [ type_ "text", value message, onInput TypeMessage ] []
        , input [ class "send", type_ "submit", value "➜" ] []
        ]


viewLanguageSelect : String -> Html Msg
viewLanguageSelect selectedLang =
    div []
        [ select [ onInput ChangeLanguage ]
            (List.map
                (\lang ->
                    option
                        [ value (Tuple.second lang)
                        , selected
                            (if Tuple.second lang == selectedLang then
                                True

                             else
                                False
                            )
                        ]
                        [ text (Tuple.first lang) ]
                )
                allLanguages
            )
        ]


viewInput : CurrentUser -> List Message -> String -> String -> Html Msg
viewInput user suggestions customMessage chosenLanguage =
    div [ class "input-area" ]
        [ viewLanguageSelect chosenLanguage
        , viewSuggestions user suggestions
        , viewCustomMessage user customMessage
        ]


view : Model -> Html Msg
view model =
    let
        name =
            case model.currentUser of
                User1 ->
                    model.user2.name

                User2 ->
                    model.user1.name
    in
    div []
        [ div [ class "app-container" ]
            [ viewInput
                model.currentUser
                model.translatedSuggestions
                model.composeMessage
                (if model.currentUser == User1 then
                    model.user1.language

                 else
                    model.user2.language
                )
            , viewMessages model.currentUser name model.messages
            ]
        , footer []
            [ button [ class "switch-user", onClick SwitchUser ] [ text "Switch User" ]
            , form [ action "https://ufl.qualtrics.com/jfe/form/SV_51oJINWTWlHX5Ii" ] [ input [ type_ "submit", value "Take Survey" ] [] ]
            ]
        ]



-- HTTP


requestEncoder : List Message -> Encode.Value
requestEncoder messages =
    Encode.list Encode.object (List.map (\message -> [ ( "Text", Encode.string message.text ) ]) messages)


translationDecoder : Decoder TranslationResult
translationDecoder =
    list (field "translations" (list (field "text" string)))


getTranslation : WhichList -> List Message -> ( String, String ) -> Cmd Msg
getTranslation whichList messages languages =
    Http.request
        { method = "POST"
        , url = "https://api.cognitive.microsofttranslator.com/translate?api-version=3.0&from=" ++ Tuple.first languages ++ "&to=" ++ Tuple.second languages
        , headers = [ header "Ocp-Apim-Subscription-Key" "0ebb2a1857ba4e499d11734856973f98" ]
        , body = Http.jsonBody (requestEncoder messages)
        , expect = Http.expectJson (GotTranslation whichList messages) translationDecoder
        , timeout = Maybe.Nothing
        , tracker = Maybe.Nothing
        }


allLanguages : List ( String, String )
allLanguages =
    [ ( "Arabic", "ar" )
    , ( "Chinese Simplified", "zh-Hans" )
    , ( "English", "en" )
    , ( "French", "fr" )
    , ( "German", "de" )
    , ( "Greek", "el" )
    , ( "Hindi", "hi" )
    , ( "Italian", "it" )
    , ( "Japanese", "ja" )
    , ( "Russian", "ru" )
    , ( "Spanish", "es" )
    ]
