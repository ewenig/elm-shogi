module Shogi.Types exposing (..)

import Dict exposing (Dict)

-- Differentiate the pieces by class
type PieceClass
    = SenteKing
    | GoteKing
    | Gold
    | Silver
    | Knight
    | Rook
    | Bishop
    | Lance
    | Pawn
    | PromotedSilver
    | PromotedKnight
    | PromotedLance
    | PromotedPawn
    | Dragon
    | Horse
    | NoPiece

type Player
    = Sente  -- Black
    | Gote   -- White

type ActiveState
    = Move
    | Drop
    | Inactive

--                 File, Rank
type alias Cell = ( Int, Int )

type alias Piece =
    { class : PieceClass
    , orientation : Player
    }

type Dialog
    = Promotion
    | Error String
    | NoDialog

type alias Model =
    -- flags
    { active : ActiveState
    , dialog : Dialog
    
    -- gameplay-relevant cells
    , activeCell : Cell
    , dropPiece : Piece

    -- game state
    , turn : Player
    , board : Board
    , senteHand : Hand
    , goteHand : Hand
    }

type Msg
    = ActivateCell Cell
    | DeactivatePiece
    | MovePiece Cell Cell Piece
    | DropPiece Piece Cell
    | UpdateDropPiece PieceClass
    | ActivateDrop

type alias Hand = List Piece

type alias Board = Dict Cell Piece

type TestResult
    = FriendlyPiece
    | EnemyPiece
    | Empty
    | OutOfBounds

