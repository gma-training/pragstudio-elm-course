module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode

import ViewHelpers exposing (..)
import Entry
import Score


-- MODEL


type GameState = EnteringName | Playing


type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry.Entry
    , alertMessage : Maybe String
    , nameInput : String
    , gameState : GameState
    }


initialModel : Model
initialModel =
    { name = "Anonymous"
    , gameNumber = 1
    , entries = []
    , alertMessage = Nothing
    , nameInput = ""
    , gameState = EnteringName
    }


-- UPDATE


type Msg
    = NewGame
    | Mark Int
    | NewRandom Int
    | NewEntries (Result Http.Error (List Entry.Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error Score.Score)
    | SetNameInput String
    | SaveName
    | CancelName
    | ChangeGameState GameState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )

        NewRandom randomNumber ->
            ( { model | gameNumber = randomNumber }, Cmd.none )

        NewGame ->
            ( { model | gameNumber = model.gameNumber + 1 }, getEntries )

        NewEntries (Ok randomEntries) ->
            ( { model | entries = randomEntries }, Cmd.none )

        NewEntries (Err error) ->
            ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )

        ShareScore ->
            ( model, postScore model.name (Entry.sumMarkedPoints model.entries) )

        NewScore (Ok score) ->
            let
                message =
                    "Your score of "
                        ++ (toString score.score)
                        ++ " was successfully shared!"
            in
                ( { model | alertMessage = Just message }, Cmd.none )

        NewScore (Err error) ->
            ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )

        Mark id ->
            ( { model | entries = Entry.markEntryWithId model.entries id }, Cmd.none )

        SetNameInput value ->
            ( { model | nameInput = value }, Cmd.none )

        SaveName ->
            if String.isEmpty model.nameInput then
                ( model, Cmd.none )
            else
                ( { model | name = model.nameInput, nameInput = "", gameState = Playing }, Cmd.none )

        CancelName ->
            ( { model | nameInput = "", gameState = Playing }, Cmd.none )

        ChangeGameState state ->
            ( { model | gameState = state }, Cmd.none )


httpErrorToMessage : Http.Error -> String
httpErrorToMessage error =
    case error of
        Http.NetworkError ->
            "Is the server running?"

        Http.BadStatus response ->
            (toString response.status)

        Http.BadPayload message _ ->
            "Decoding Failed: " ++ message

        _ ->
            (toString error)


-- COMMANDS


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)


apiUrlPrefix : String
apiUrlPrefix =
    "http://localhost:3000"


postScore : String -> Int -> Cmd Msg
postScore name score =
    Score.postScore NewScore (apiUrlPrefix ++ "/scores") name score


getEntries : Cmd Msg
getEntries =
    Entry.getEntries NewEntries (apiUrlPrefix ++ "/random-entries")


-- VIEW


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , alert CloseAlert model.alertMessage
        , viewNameInput model
        , Entry.viewEntryList Mark model.entries
        , Score.viewScore (Entry.sumMarkedPoints model.entries)
        , div [ class "button-group" ]
            [ primaryButton NewGame "New Game"
            , primaryButton ShareScore "Share Score"
            ]
        --, div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]


viewNameInput : Model -> Html Msg
viewNameInput model =
    case model.gameState of
        EnteringName ->
            div [ class "name-input" ]
                [ input
                    [ type_ "text"
                    , placeholder "Who's playing?"
                    , value model.nameInput
                    , autofocus True
                    , onInput SetNameInput
                    ]
                    []
                , primaryButton SaveName "Save"
                , primaryButton CancelName "Cancel"
                ]

        _ ->
            text ""


viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    h2 [ id "info", class "classy" ]
        [ a [ href "#", onClick (ChangeGameState EnteringName) ]
            [ text name ]
        , text (" - Game #" ++ (toString gameNumber))
        ]


viewHeader : String -> Html Msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ a [ href "http://elm-lang.org" ]
            [ text "Powered By Elm" ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, getEntries )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
