module Shogi exposing (..)

import Html exposing (Html)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import List exposing (map, concatMap, filterMap, head, range)
import Tuple exposing (first, second)
import Dict
import Maybe

import Shogi.Types exposing (..)
import Shogi.Board exposing (..)
import Shogi.Piece exposing (..)
import Shogi.View exposing (..)

-- App declaration
main =
    Html.program
        { init = init
        , view = Shogi.View.view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }

-- Model logic

defaultCell : Cell
defaultCell = ( 0, 0 )

init : (Model, Cmd Msg)
init =
    let
        initModel : Model
        initModel =
            { active = Inactive
            , dialog = NoDialog
            , activeCell = defaultCell
            , dropPiece = defaultPiece
            , turn = Sente
            , board = Dict.fromList initBoard
            , senteHand = []
            , goteHand = []
            }
    in
        ( initModel, Cmd.none )


-- Update logic

switchTurn : Player -> Player
switchTurn player =
    case player of
        Sente -> Gote
        Gote -> Sente

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ActivateCell cell ->
            let
                piece : Piece
                piece = getPiece model.board cell
            in
                -- don't activate the cell if it's not that player's turn
                if piece.orientation == model.turn then
                    ({ model | active = Move, activeCell = cell }, Cmd.none)
                else 
                    (model, Cmd.none)

        DeactivatePiece ->
            ({ model | active = Inactive, activeCell = defaultCell }, Cmd.none)

        MovePiece oldCell newCell piece ->
            let
                takes : Maybe Piece
                takes = Dict.get newCell model.board

                newBoard : Board
                newBoard =
                    Dict.insert newCell piece
                        <| Dict.remove oldCell model.board

                newHands : (Hand, Hand)
                newHands = 
                    let
                        sente : Hand
                        sente = model.senteHand

                        gote : Hand
                        gote = model.goteHand

                    in
                        case takes of
                            Just piece ->
                                case model.turn of
                                    Sente -> ( ( postCapture <| piece ) :: sente, gote )
                                    Gote -> ( sente, ( postCapture <| piece ) :: gote )
                            Nothing -> ( sente, gote )
            in
                ({ model
                    | board = newBoard
                    , activeCell = defaultCell
                    , turn = switchTurn model.turn
                    , senteHand = first newHands
                    , goteHand = second newHands
                    }
                , Cmd.none)

        DropPiece newPiece cell ->
            let
                newBoard : Board
                newBoard = Dict.insert cell newPiece model.board

                newHands : (Hand, Hand)
                newHands =
                    let
                        hand : Hand
                        hand =
                            case model.turn of
                                Sente -> model.senteHand
                                Gote -> model.goteHand

                        hands : Hand -> (Hand, Hand)
                        hands hand_ =
                            case model.turn of
                                Sente -> (hand_, model.goteHand)
                                Gote -> (model.senteHand, hand_)

                        filterOne : (Piece -> Bool) -> Hand -> Hand
                        filterOne predicate list =
                            let
                                hands_ : (Hand, Hand)
                                hands_ = List.partition predicate hand
                            in
                                List.append ( Maybe.withDefault [] <| List.tail <| first hands_ ) ( second hands_ )
                    in
                        hands <| filterOne (\piece -> piece == newPiece) hand
                        
            in
                ( { model
                    | board = newBoard
                    , turn = switchTurn model.turn
                    , active = Inactive
                    , dropPiece = defaultPiece
                    , senteHand = first newHands
                    , goteHand = second newHands
                    }
                , Cmd.none)

        UpdateDropPiece piece ->
            ( { model | dropPiece = Piece piece model.turn, active = Inactive }, Cmd.none )

        ActivateDrop ->
            if model.dropPiece.class == NoPiece then
                -- nop
                ( model, Cmd.none )
            else 
                ( { model | active = Drop }, Cmd.none )

