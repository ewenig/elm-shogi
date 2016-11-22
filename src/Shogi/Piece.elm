module Shogi.Piece exposing (..)

import List exposing (..)
import Dict

import Shogi.Types exposing (..)
import Shogi.Board exposing (rank, file)
import Tuple exposing (first, second)

-- Piece logic

defaultPiece : Piece
defaultPiece =
    { class = NoPiece
    , orientation = Sente
    }

getPiece : Board -> Cell -> Piece
getPiece board cell =
    case Dict.get cell board of
        Just piece -> piece
        Nothing -> defaultPiece

promotePiece : Piece -> Piece
promotePiece piece =
    let newClass =
        case piece.class of
            Silver -> PromotedSilver
            Knight -> PromotedKnight
            Rook -> Dragon
            Bishop -> Horse
            Lance -> PromotedLance
            Pawn -> PromotedPawn
            _ -> NoPiece
    in
       { piece | class = newClass }

-- Flip & unpromote a piece after it gets captured
postCapture : Piece -> Piece
postCapture piece =
    let
        unpromotePiece : Piece -> Piece
        unpromotePiece piece =
            let newClass =
                case piece.class of
                    PromotedSilver -> Silver
                    PromotedKnight -> PromotedKnight
                    Dragon -> Rook
                    Horse -> Bishop
                    PromotedLance -> Lance
                    PromotedPawn -> Pawn
                    _ -> piece.class
            in
               { piece | class = newClass }

        flipPiece : Piece -> Piece
        flipPiece piece =
            let
                newOrientation : Player
                newOrientation =
                    case piece.orientation of
                        Sente -> Gote
                        Gote -> Sente
            in
                { piece | orientation = newOrientation }
    in
        flipPiece <| unpromotePiece piece

isPromoted : Piece -> Bool
isPromoted piece =
    let
        promotedPieces = [ PromotedSilver, PromotedKnight, Dragon, Horse, PromotedLance, PromotedPawn ]
    in
        List.member piece.class promotedPieces

isPromotable : Piece -> Bool
isPromotable piece =
    let
        promotablePieces = [ Silver, Knight, Rook, Bishop, Lance, Pawn ]
    in
        List.member piece.class promotablePieces

-- test one individual cell for a piece
cellTest : Board -> Player -> Cell -> TestResult
cellTest board turn cell =
    let
        isOutOfBounds : Bool
        isOutOfBounds = 
            (rank cell > 9 || rank cell < 1 || file cell > 9 || file cell < 1 )
    in
        if isOutOfBounds then
            OutOfBounds
        else
            case Dict.get cell board of
                Just piece ->
                    if turn == piece.orientation then
                        FriendlyPiece
                    else
                        EnemyPiece
                Nothing -> Empty

rayTest : Board -> Player -> (Cell -> Cell) -> Cell -> List Cell
rayTest board turn advance cell =
    let
        nextCell : Cell
        nextCell = advance cell
    in
        case cellTest board turn nextCell of
            Empty -> nextCell :: rayTest board turn advance nextCell
            EnemyPiece -> [ nextCell ]
            _ -> []

-- Return all the legal moves given a board state and active cell
legalMoves : Board -> Cell -> Player -> List Cell
legalMoves board activeCell turn =
    let
        rayTest_ : (Cell -> Cell) -> Cell -> List Cell
        rayTest_ = rayTest board turn

        legalMoveFilter : (Cell, TestResult) -> Maybe Cell
        legalMoveFilter tuple =
            let
                cell = first tuple
                result = second tuple
            in
                case result of
                    Empty -> Just cell
                    EnemyPiece -> Just cell
                    _ -> Nothing

        piece : Piece
        piece = getPiece board activeCell

        direction : Int
        direction =
            case piece.orientation of
                Sente -> -1
                Gote -> 1

        lanceTest : List Cell
        lanceTest =
            let
                test : Cell -> Cell
                test cell = ( file cell, rank cell + direction )
            in
                rayTest_ test activeCell

        bishopTest : List Cell
        bishopTest =
            let
                test1 : Cell -> Cell
                test1 cell = ( file cell + 1, rank cell + 1 )
                test2 : Cell -> Cell
                test2 cell = ( file cell - 1, rank cell + 1 )
                test3 : Cell -> Cell
                test3 cell = ( file cell + 1, rank cell - 1 )
                test4 : Cell -> Cell
                test4 cell = ( file cell - 1 , rank cell - 1 )
            in
                rayTest_ test1 activeCell ++
                rayTest_ test2 activeCell ++
                rayTest_ test3 activeCell ++
                rayTest_ test4 activeCell
                
        rookTest : List Cell
        rookTest =
            let
                test1 : Cell -> Cell
                test1 cell = ( file cell + 1, rank cell )
                test2 : Cell -> Cell
                test2 cell = ( file cell, rank cell + 1 )
                test3 : Cell -> Cell
                test3 cell = ( file cell, rank cell - 1 )
                test4 : Cell -> Cell
                test4 cell = ( file cell - 1, rank cell )
            in
                rayTest_ test1 activeCell ++
                rayTest_ test2 activeCell ++
                rayTest_ test3 activeCell ++
                rayTest_ test4 activeCell

        genericTest : List Cell -> List Cell
        genericTest possibleMoves =
            let
                moveResults : List TestResult
                moveResults =
                    List.map ( cellTest board turn ) possibleMoves
            in
                filterMap legalMoveFilter <| List.map2 (,) possibleMoves moveResults

        frontTest : List Cell
        frontTest =
            genericTest
                [ ( file activeCell + 1, rank activeCell + direction )
                , ( file activeCell,  rank activeCell + direction )
                , ( file activeCell - 1, rank activeCell + direction )
                ]

        sideTest : List Cell
        sideTest = 
            genericTest
                [ ( file activeCell + 1, rank activeCell )
                , ( file activeCell - 1, rank activeCell )
                ]

        goldTest : List Cell
        goldTest =
            frontTest ++ sideTest ++
            genericTest
                [ ( file activeCell, rank activeCell - direction ) ]

        silverTest : List Cell
        silverTest =
            frontTest ++
            genericTest
                [ ( file activeCell + 1, rank activeCell - direction )
                , ( file activeCell - 1, rank activeCell - direction )
                ]

        kingTest : List Cell
        kingTest =
            frontTest ++ sideTest ++
            genericTest
                [ ( file activeCell + 1, rank activeCell - direction )
                , ( file activeCell, rank activeCell - direction )
                , ( file activeCell - 1, rank activeCell - direction )
                ]

        knightTest : List Cell
        knightTest =
            genericTest
                [ ( file activeCell + 1, rank activeCell + direction * 2 )
                , ( file activeCell - 1, rank activeCell + direction * 2 )
                ]

    in
        case piece.class of
            Pawn -> genericTest [ ( file activeCell, rank activeCell + direction ) ]
            Lance -> lanceTest
            Knight -> knightTest
            Silver -> silverTest
            Gold -> goldTest
            SenteKing -> kingTest
            GoteKing -> kingTest
            Rook -> rookTest
            Bishop -> bishopTest
            PromotedPawn -> goldTest
            PromotedLance -> goldTest
            PromotedKnight -> goldTest
            PromotedSilver -> goldTest
            Dragon -> rookTest ++ kingTest
            Horse -> bishopTest ++ kingTest
            _ -> []

legalDrops : Board -> Piece -> Player -> List Cell
legalDrops board piece turn =
    let
        files : List Int
        files = List.range 1 9

        emptyBoard : List Cell
        emptyBoard =
            ( List.range 1 9 ) |> List.concatMap (\a -> ( List.range 1 9 ) |> List.map (\b -> (a, b) )) 

        emptyFile : Int -> List Cell
        emptyFile file =
            List.map ( \x -> (file, x) ) <| List.range 1 9

        pawnInFile : Int -> Bool
        pawnInFile file =
            let
                friendlyPawnInCell : Cell -> Bool
                friendlyPawnInCell cell =
                    let
                        piece : Piece
                        piece = getPiece board cell
                    in
                        piece.class == Pawn && piece.orientation == turn
            in
                List.length ( List.filter friendlyPawnInCell <| emptyFile file ) > 0
        
        legalPawnDropsInFile : Int -> List Cell
        legalPawnDropsInFile file =
            if pawnInFile file then
                []
            else
                List.filter ( \cell -> ( cellTest board turn ) cell == Empty ) <| emptyFile file
    in
        case piece.class of
            -- No pawn drop for now
            Pawn -> List.concatMap legalPawnDropsInFile <| List.range 1 9
            NoPiece -> []
            _ -> List.filter ( \cell -> ( cellTest board turn ) cell == Empty ) emptyBoard

