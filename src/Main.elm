module Main exposing (Message, Model, Msg(..), Status(..), Suggestion, init, main, update, view, viewCustomMessage, viewInput, viewMessage, viewMessages, viewSuggestions)

import Browser
import Html exposing (Html, br, button, div, footer, form, h1, input, label, li, p, text, ul)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
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


type alias Suggestion =
    { text : String, status : Status }


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
    , suggestions : List Suggestion
    , currentStatus : Status
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { user1 = { name = "Zhang", language = "en" }
      , user2 = { name = "Zach", language = "en" }
      , currentUser = User1
      , messages = []
      , composeMessage = ""
      , suggestions =
            [ Suggestion "I understand" Understand
            , Suggestion "I am at my destination" Understand
            , Suggestion "Can you give more detail?" Confused
            , Suggestion "I do not see it" Confused
            , Suggestion "That does not make sense" Confused
            ]
      , currentStatus = Neutral
      }
    , Cmd.none
    )



-- UPDATE


type Status
    = Understand
    | Confused
    | Neutral


type alias Message =
    { sender : CurrentUser, status : Status, text : String }


type alias TranslationResult =
    List (List String)


type Msg
    = SwitchUser
    | ChangeLanguage String
    | SendMessage Message
    | GotTranslation (Result Http.Error TranslationResult)
    | TypeMessage String


textFromTranslation : TranslationResult -> String
textFromTranslation translation =
    case List.head translation of
        Just outer ->
            case List.head outer of
                Just inner ->
                    inner

                Nothing ->
                    "Translation failed"

        Nothing ->
            "Translation failed"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SwitchUser ->
            case model.currentUser of
                User1 ->
                    ( { model | currentUser = User2 }, Cmd.none )

                User2 ->
                    ( { model | currentUser = User1 }, Cmd.none )

        ChangeLanguage newLanguage ->
            case model.currentUser of
                User1 ->
                    let
                        updated =
                            model.user1
                    in
                    ( { model | user1 = { updated | language = newLanguage } }, Cmd.none )

                User2 ->
                    let
                        updated =
                            model.user2
                    in
                    ( { model | user2 = { updated | language = newLanguage } }, Cmd.none )

        SendMessage message ->
            ( { model | composeMessage = "", currentStatus = message.status }, getTranslation message.text )

        GotTranslation result ->
            case result of
                Ok translation ->
                    ( { model | messages = List.append model.messages [ { sender = model.currentUser, status = model.currentStatus, text = textFromTranslation translation } ] }, Cmd.none )

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
    in
    div [ class (messageClass ++ "-wrapper") ]
        [ p [ class ("message " ++ messageClass) ] [ text message.text ]
        ]


viewMessages : CurrentUser -> String -> List Message -> Html Msg
viewMessages user name messages =
    div [ class "chat-area" ]
        [ h1 [ class "chat-name" ] [ text name ]
        , div [ class "messages" ] (List.map (viewMessage user) messages)
        ]


viewSuggestions : CurrentUser -> List Suggestion -> Html Msg
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
        [ label [] [ text "Message" ]
        , br [] []
        , input [ type_ "text", value message, onInput TypeMessage ] []
        , input [ type_ "submit", value "Send" ] []
        ]


viewInput : CurrentUser -> List Suggestion -> String -> Html Msg
viewInput user suggestions customMessage =
    div [ class "input-area" ]
        [ viewSuggestions user suggestions
        , viewCustomMessage user customMessage
        ]


view : Model -> Html Msg
view model =
    let
        name =
            case model.currentUser of
                User1 ->
                    model.user1.name

                User2 ->
                    model.user2.name
    in
    div []
        [ div [ class "app-container" ]
            [ viewMessages model.currentUser name model.messages
            , viewInput model.currentUser model.suggestions model.composeMessage
            ]
        , footer [] [ button [ class "switch-user", onClick SwitchUser ] [ text "Switch User" ] ]
        ]



-- HTTP


requestEncoder : String -> Encode.Value
requestEncoder message =
    Encode.list Encode.object [ [ ( "Text", Encode.string message ) ] ]


translationDecoder : Decoder TranslationResult
translationDecoder =
    list (field "translations" (list (field "text" string)))


getTranslation : String -> Cmd Msg
getTranslation message =
    Http.request
        { method = "POST"
        , url = "https://api.cognitive.microsofttranslator.com/translate?api-version=3.0&from=en&to=zh-Hans"
        , headers = [ header "Ocp-Apim-Subscription-Key" "0ebb2a1857ba4e499d11734856973f98" ]
        , body = Http.jsonBody (requestEncoder message)
        , expect = Http.expectJson GotTranslation translationDecoder
        , timeout = Maybe.Nothing
        , tracker = Maybe.Nothing
        }
