module Shogi.Board exposing (..)

import Html exposing (Html)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)

import Shogi.Types exposing (..)

rank : Cell -> Int
rank cell = second cell

file : Cell -> Int
file cell = first cell

displayBoard : List (Html Msg)
displayBoard =
    let
        gridColor = "#000000"
        boardColor = "#E9C27F"
    in
        [ rect [ x "0", y "0", width "900", height "900", fill boardColor, Html.Attributes.style [ ("-webkit-touch-callout", "none"), ("-webkit-user-select", "none"), ("-khtml-user-select", "none"), ("-moz-user-select", "none"), ("-ms-user-select", "none"), ("user-select", "none") ] ] []
        , line [ x1 "0",   y1 "0",   x2 "900", y2 "0",   stroke gridColor ] []
        , line [ x1 "0",   y1 "100", x2 "900", y2 "100", stroke gridColor ] []
        , line [ x1 "0",   y1 "200", x2 "900", y2 "200", stroke gridColor ] []
        , line [ x1 "0",   y1 "300", x2 "900", y2 "300", stroke gridColor ] []
        , line [ x1 "0",   y1 "400", x2 "900", y2 "400", stroke gridColor ] []
        , line [ x1 "0",   y1 "500", x2 "900", y2 "500", stroke gridColor ] []
        , line [ x1 "0",   y1 "600", x2 "900", y2 "600", stroke gridColor ] []
        , line [ x1 "0",   y1 "700", x2 "900", y2 "700", stroke gridColor ] []
        , line [ x1 "0",   y1 "800", x2 "900", y2 "800", stroke gridColor ] []
        , line [ x1 "0",   y1 "900", x2 "900", y2 "900", stroke gridColor ] []
        , line [ x1 "0",   y1 "0",   x2 "0",   y2 "900", stroke gridColor ] []
        , line [ x1 "100", y1 "0",   x2 "100", y2 "900", stroke gridColor ] []
        , line [ x1 "200", y1 "0",   x2 "200", y2 "900", stroke gridColor ] []
        , line [ x1 "300", y1 "0",   x2 "300", y2 "900", stroke gridColor ] []
        , line [ x1 "400", y1 "0",   x2 "400", y2 "900", stroke gridColor ] []
        , line [ x1 "500", y1 "0",   x2 "500", y2 "900", stroke gridColor ] []
        , line [ x1 "600", y1 "0",   x2 "600", y2 "900", stroke gridColor ] []
        , line [ x1 "700", y1 "0",   x2 "700", y2 "900", stroke gridColor ] []
        , line [ x1 "800", y1 "0",   x2 "800", y2 "900", stroke gridColor ] []
        , line [ x1 "900", y1 "0",   x2 "900", y2 "900", stroke gridColor ] []
        ]

initBoard : List (Cell, Piece)
initBoard =
    [ ((1, 7), Piece Pawn Sente)
    , ((2, 7), Piece Pawn Sente)
    , ((3, 7), Piece Pawn Sente)
    , ((4, 7), Piece Pawn Sente)
    , ((5, 7), Piece Pawn Sente)
    , ((6, 7), Piece Pawn Sente)
    , ((7, 7), Piece Pawn Sente)
    , ((8, 7), Piece Pawn Sente)
    , ((9, 7), Piece Pawn Sente)
    , ((8, 8), Piece Bishop Sente)
    , ((2, 8), Piece Rook Sente)
    , ((1, 9), Piece Lance Sente)
    , ((2, 9), Piece Knight Sente)
    , ((3, 9), Piece Silver Sente)
    , ((4, 9), Piece Gold Sente)
    , ((5, 9), Piece SenteKing Sente)
    , ((6, 9), Piece Gold Sente)
    , ((7, 9), Piece Silver Sente)
    , ((8, 9), Piece Knight Sente)
    , ((9, 9), Piece Lance Sente)
    , ((1, 3), Piece Pawn Gote)
    , ((2, 3), Piece Pawn Gote)
    , ((3, 3), Piece Pawn Gote)
    , ((4, 3), Piece Pawn Gote)
    , ((5, 3), Piece Pawn Gote)
    , ((6, 3), Piece Pawn Gote)
    , ((7, 3), Piece Pawn Gote)
    , ((8, 3), Piece Pawn Gote)
    , ((9, 3), Piece Pawn Gote)
    , ((2, 2), Piece Bishop Gote)
    , ((8, 2), Piece Rook Gote)
    , ((1, 1), Piece Lance Gote)
    , ((2, 1), Piece Knight Gote)
    , ((3, 1), Piece Silver Gote)
    , ((4, 1), Piece Gold Gote)
    , ((5, 1), Piece GoteKing Gote)
    , ((6, 1), Piece Gold Gote)
    , ((7, 1), Piece Silver Gote)
    , ((8, 1), Piece Knight Gote)
    , ((9, 1), Piece Lance Gote)
    ]

