module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


-- MODEL


initialModel =
    { name = "Mike"
    , gameNumber = 1
    , entries = initialEntries
    }


initialEntries =
    [ { id = 1, phrase = "Future-Proof", points = 100, marked = False }
    , { id = 2, phrase = "Doing Agile", points = 200, marked = False }
    ]



-- VIEW


playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " - Game #" ++ (toString gameNumber)


viewPlayer : String -> Int -> Html msg
viewPlayer name gameNumber =
    let
        playerInfoText =
            playerInfo name gameNumber
                |> String.toUpper
                |> text
    in
        h2 [ id "info", class "classy" ]
            [ playerInfoText ]


viewHeader : String -> Html msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]


viewFooter : Html msg
viewFooter =
    footer []
        [ a [ href "http://elm-lang.org" ]
            [ text "Powered By Elm" ]
        ]



-- view : Html msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]


main : Html msg
main =
    view initialModel
