module Shogi.View exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (targetValue)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Json.Decode
import List
import String exposing (slice)
import Tuple exposing (first, second)
import Dict

import Shogi.Types exposing (..)
import Shogi.Piece exposing (..)
import Shogi.Board exposing (displayBoard, file, rank)

-- View logic

kanjiByClass : PieceClass -> String
kanjiByClass class =
    case class of
        SenteKing -> "玉"
        GoteKing -> "王"
        Rook -> "飛"
        Dragon -> "龍"
        Bishop -> "角"
        Horse -> "龍"
        Gold -> "金"
        Silver -> "銀"
        PromotedSilver -> "成"
        Knight -> "桂"
        PromotedKnight -> "成"
        Lance -> "香"
        PromotedLance -> "成"
        Pawn -> "歩"
        PromotedPawn -> "と"
        NoPiece -> "E"

baseX : Cell -> Int
baseX cell =
    ( 9 - file cell ) * 100

baseY : Cell -> Int
baseY cell =
    ( rank cell - 1 ) * 100

rotation : (Cell, Piece) -> String
rotation tuple =
    let 
        cell : Cell
        cell = first tuple

        piece : Piece
        piece = second tuple

        degrees : Int
        degrees =
            case piece.orientation of
               Sente -> 0
               Gote -> 180
    in
        "rotate(" ++ toString degrees ++ " " ++ toString ( baseX cell + 50 ) ++ " " ++ toString ( baseY cell + 50 ) ++ ")"

displayPieceText : (Cell, Piece) -> Svg Msg
displayPieceText tuple =
    let
        cell : Cell
        cell = first tuple

        piece : Piece
        piece = second tuple

        textColor : String
        textColor =
            if isPromoted piece then
               "red"
            else
                "black"
    in
        text_ [ x <| toString ( baseX cell + 24 )
              , y <| toString ( baseY cell + 75 )
              , fontSize "50"
              , fontWeight "bold"
              , fill textColor
              , pointerEvents "none"
              , transform <| rotation tuple
              ] [ text <| kanjiByClass piece.class ]

displayPieceShape : Cell -> (Cell, Piece) -> Svg Msg
displayPieceShape activeCell tuple =
    let
        cell : Cell
        cell = first tuple

        piece : Piece
        piece = second tuple

        pieceColor = "#FFE3B3"

        targetMsg : Msg
        targetMsg =
            if activeCell == cell then
                DeactivatePiece
            else
                ActivateCell cell
    in
        polygon [ points ( toString ( baseX cell + 50 ) ++ "," ++ toString ( baseY cell + 10 ) ++ " " ++
                           toString ( baseX cell + 75 ) ++ "," ++ toString ( baseY cell + 30 ) ++ " " ++
                           toString ( baseX cell + 80 ) ++ "," ++ toString ( baseY cell + 90 ) ++ " " ++
                           toString ( baseX cell + 20 ) ++ "," ++ toString ( baseY cell + 90 ) ++ " " ++
                           toString ( baseX cell + 25 ) ++ "," ++ toString ( baseY cell + 30 )
                         )
                , stroke "#000000"
                , fill pieceColor
                , strokeWidth "3"
                , transform <| rotation tuple
                , onClick targetMsg
                ] []

displayPossibleMoves : Model -> List (Svg Msg)
displayPossibleMoves model =
    let
        message : Cell -> Board -> Msg
        message cell board =
            let
                piece : Piece
                piece = getPiece board model.activeCell
            in
                case model.active of
                    Drop -> DropPiece model.dropPiece cell
                    _ -> MovePiece model.activeCell cell piece

        placeMarker : Cell -> Svg Msg
        placeMarker cell =
            circle
                [ cx <| toString ( baseX cell + 50 )
                , cy <| toString ( baseY cell + 50 )
                , r "20"
                , fill "red"
                , fillOpacity "0.3"
                , onClick <| message cell model.board
                ] []

    in
        case model.active of
            Move -> List.map placeMarker <| legalMoves model.board model.activeCell model.turn
            Drop -> List.map placeMarker <| legalDrops model.board model.dropPiece model.turn
            Inactive -> []

handOptions : Hand -> List (Html Msg)
handOptions hand =
    let
        pieceToOption : Piece -> Html Msg
        pieceToOption piece =
            Html.option
                [ Html.Attributes.value <| toString piece.class ]
                [ text <| toString piece.class ]
    in
        List.map pieceToOption hand

dropPieceDecoder : Json.Decode.Decoder PieceClass
dropPieceDecoder =
    targetValue |> Json.Decode.andThen
        (\val ->
            case val of
                "Pawn" -> Json.Decode.succeed Pawn
                "Lance" -> Json.Decode.succeed Lance
                "Knight" -> Json.Decode.succeed Knight
                "Gold" -> Json.Decode.succeed Gold
                "Silver" -> Json.Decode.succeed Silver
                "Bishop" -> Json.Decode.succeed Bishop
                "Rook" -> Json.Decode.succeed Rook
                _ -> Json.Decode.succeed NoPiece
        )


view : Model -> Html Msg
view model =
    let
        currentHand : Hand
        currentHand =
            case model.turn of
                Sente -> model.senteHand
                Gote -> model.goteHand

        boardList : List (Cell, Piece)
        boardList = Dict.toList model.board
    in
        Html.body []
            [ Html.div [ Html.Attributes.style [ ("margin", "10px"), ("border", "3px solid lightgrey"), ("padding", "5px 5px 5px 5px"), ("width", "300px"), ("position", "fixed"), ("top", "0"), ("left", "0"), ("z-index", "1") ] ]
                [ Html.pre [ Html.Attributes.style [ ("margin-top", "0"), ("white-space", "pre-wrap") ] ] [ text <| statusDebugText model ]
                , displayHands model
                , Html.select [ Html.Attributes.style [ ("width", "100%") ], on "change" <| Json.Decode.map UpdateDropPiece dropPieceDecoder ] <| handOptions currentHand
                , Html.button [ Html.Attributes.style [ ("width", "100%") ], onClick ActivateDrop ] [ text "Drop" ]
                ],
             svg
                [ viewBox "0 0 900 900"
                , width "900"
                -- fill the whole screen
                , Html.Attributes.style [ ("position", "fixed"), ("top", "0"), ("left", "0"), ("z-index", "0") ]
                , width "100%"
                , height "100%"
                ]
                (
                displayBoard ++
                List.map ( displayPieceShape model.activeCell ) boardList ++
                List.map displayPieceText boardList ++
                displayPossibleMoves model
                )
                ]

-- Debug routines
statusDebugText : Model -> String
statusDebugText model =
    let
        activeText : String
        activeText =
            if model.active /= Inactive then
                "Cell is active: " ++ toString model.activeCell ++ ". Legal moves are: " ++ toString ( legalMoves model.board model.activeCell model.turn )

            else
                "No cell is active."

    in
        toString model.turn ++ "'s turn. " ++ activeText

displayHands : Model -> Html Msg
displayHands model =
    let
        handString : Hand -> String
        handString hand =
            let
                pieceString : Piece -> String -> String
                pieceString piece string = string ++ toString piece.class ++ ", "
            in
                slice 0 -2 <| List.foldr pieceString "" hand

        sente : String
        sente = handString model.senteHand
        gote : String
        gote = handString model.goteHand
    in 
        Html.div [ id "hands" ]
            [ Html.pre [ Html.Attributes.style [ ("white-space", "pre-wrap") ] ] [ text <| "Sente's hand: " ++ sente ]
            , Html.pre [ Html.Attributes.style [ ("white-space", "pre-wrap") ] ] [ text <| "Gote's hand: " ++ gote ]
            ]

