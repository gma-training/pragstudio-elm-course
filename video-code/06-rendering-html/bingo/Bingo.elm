module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


playerInfo name gameNumber =
    name ++ " - Game #" ++ (toString gameNumber)


viewPlayer name gameNumber =
    let
        playerInfoText =
            playerInfo name gameNumber
                |> String.toUpper
                |> text
    in
        h2 [ id "info", class "classy" ]
            [ playerInfoText ]


viewHeader title =
    header []
        [ h1 [] [ text title ] ]


viewFooter =
    footer []
        [ a [ href "http://elm-lang.org" ]
            [ text "Powered By Elm" ]
        ]


view =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer "Nicole" 4
        , viewFooter
        ]


main =
    view
