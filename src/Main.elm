module Main exposing (Message, Model, Msg(..), Status(..), Suggestion, init, main, update, view, viewCustomMessage, viewInput, viewMessage, viewMessages, viewSuggestions)

import Browser
import Html exposing (Html, br, div, form, input, label, li, p, text, ul)
import Html.Attributes exposing (class, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)



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


type alias Model =
    { name : String
    , messages : List Message
    , composeMessage : String
    , suggestions : List Suggestion
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { name = "Zhang"
      , messages = []
      , composeMessage = ""
      , suggestions =
            [ Suggestion "I understand" Understand
            , Suggestion "I am at my destination" Understand
            , Suggestion "Can you give more detail?" Confused
            , Suggestion "I do not see it" Confused
            , Suggestion "That does not make sense" Confused
            ]
      }
    , Cmd.none
    )



-- UPDATE


type Status
    = Understand
    | Confused
    | Neutral


type alias Message =
    { status : Status, text : String }


type Msg
    = SendMessage Message
    | TypeMessage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendMessage message ->
            ( { model | messages = List.append model.messages [ message ], composeMessage = "" }, Cmd.none )

        TypeMessage newInput ->
            ( { model | composeMessage = newInput }, Cmd.none )



-- VIEW


viewMessage : Message -> Html Msg
viewMessage message =
    div []
        [ p [] [ text message.text ]
        , p [] []
        ]


viewMessages : List Message -> Html Msg
viewMessages messages =
    div [ class "messages-area" ] (List.map viewMessage messages)


viewSuggestions : List Suggestion -> Html Msg
viewSuggestions suggestions =
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
                                suggestion.status
                                suggestion.text
                    ]
                    [ text suggestion.text ]
            )
            suggestions
        )


viewCustomMessage : String -> Html Msg
viewCustomMessage message =
    form [ onSubmit (SendMessage { status = Neutral, text = message }) ]
        [ label [] [ text "Message" ]
        , br [] []
        , input [ type_ "text", value message, onInput TypeMessage ] []
        , input [ type_ "submit", value "Send" ] []
        ]


viewInput : List Suggestion -> String -> Html Msg
viewInput suggestions customMessage =
    div [ class "input-area" ]
        [ viewSuggestions suggestions
        , viewCustomMessage customMessage
        ]


view : Model -> Html Msg
view model =
    div [ class "app-container" ]
        [ viewMessages model.messages
        , viewInput model.suggestions model.composeMessage
        ]
