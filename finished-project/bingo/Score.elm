module Score exposing (Score, viewScore, postScore)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode


type alias Score =
    { id : Int
    , name : String
    , score : Int
    }


viewScore : Int -> Html msg
viewScore sum =
    div
        [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]


postScore : (Result Http.Error Score -> msg) -> String -> String -> Int -> Cmd msg
postScore msg url name score =
    let
        body =
            encodeScore name score
                |> Http.jsonBody

        request =
            Http.post url body scoreDecoder
    in
        Http.send msg request



-- PRIVATE HELPERS --


scoreDecoder : Decoder Score
scoreDecoder =
    Decode.map3 Score
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "score" Decode.int)


encodeScore : String -> Int -> Encode.Value
encodeScore name score =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "score", Encode.int score )
        ]
