module HChess.Core.AlgebraicNotation
  ( StdNotationMove (..),
    StdNotationRegularMove (..),
    StdNotationEnPassant (..),
    StdNotationPawnPromotion (..),
    getStdNotationForGame,
    getStdNotationForLegalMove,
  )
where

import Data.Foldable (toList)
import Data.Maybe (fromJust)
import HChess.Core.Board
  ( File,
    Rank,
    Square,
    file,
    getCapturedPieces,
    getPieceAt,
    isSquareEmpty,
    rank,
  )
import HChess.Core.Check (isPlayerInCheck)
import HChess.Core.Color (oppositeColor)
import HChess.Core.Game (Game, getBoard, getMoves, oneMoveBack)
import HChess.Core.LegalMoves (getAllLegalMoves)
import HChess.Core.Move
  ( Move (..),
    MoveType (..),
    getMoveSrcSquare,
    performLegalMoveOnBoard,
  )
import HChess.Core.Piece (Piece (Piece, color, pieceType), PieceType (Pawn), pieceToUnicode)

data StdNotationMove
  = SNRegularMove !StdNotationRegularMove
  | SNEnPassant !StdNotationEnPassant
  | SNPawnPromotion !StdNotationPawnPromotion
  | SNKingsideCastling
  | SNQueensideCastling

-- TODO: Try using a better solution for the records here. Something more modern ({-# LANGUAGE RecordWildCards #-} {-# LANGUAGE NamedFieldPuns #-}) or split them into files.

data StdNotationRegularMove = StdNotationRegularMove
  { piece :: !Piece,
    srcFile :: !(Maybe File),
    srcRank :: !(Maybe Rank),
    isCapture :: !Bool,
    dstSquare :: !Square,
    isCheck :: !Bool
  }

data StdNotationEnPassant = StdNotationEnPassant
  { srcFile :: !File,
    dstSquare :: !Square
  }

data StdNotationPawnPromotion = StdNotationPawnPromotion
  { srcFile :: !(Maybe File),
    isCapture :: !Bool,
    dstSquare :: !Square,
    newPiece :: !Piece,
    isCheck :: !Bool
  }

instance Show StdNotationMove where
  show SNKingsideCastling = "O-O"
  show SNQueensideCastling = "O-O-O"
  show (SNEnPassant move) =
    show move.srcFile <> captureSymbol <> show move.dstSquare <> " e.p."
  show (SNPawnPromotion move) =
    maybe "" show move.srcFile
      <> (if move.isCapture then captureSymbol else "")
      <> show move.dstSquare
      <> "="
      <> pieceToUnicode move.newPiece
      <> (if move.isCheck then checkSymbol else "")
  show (SNRegularMove move) =
    ( if move.piece.pieceType == Pawn
        then ""
        else pieceToUnicode move.piece
    )
      <> maybe "" show move.srcFile
      <> maybe "" show move.srcRank
      <> (if move.isCapture then captureSymbol else "")
      <> show move.dstSquare
      <> (if move.isCheck then checkSymbol else "")

captureSymbol :: String
captureSymbol = "x"

checkSymbol :: String
checkSymbol = "+"

-- | First move is the latest.
getStdNotationForGame :: Game -> [StdNotationMove]
getStdNotationForGame game = case getMoves game of
  [] -> []
  (move : _) ->
    let game' = oneMoveBack game
     in getStdNotationForLegalMove game' move : getStdNotationForGame game'

getStdNotationForLegalMove :: Game -> Move -> StdNotationMove
getStdNotationForLegalMove game move@(Move srcSquare dstSquare moveType) =
  case moveType of
    KingsideCastling -> SNKingsideCastling
    QueensideCastling -> SNQueensideCastling
    EnPassant -> SNEnPassant $ StdNotationEnPassant {srcFile = srcSquare.file, dstSquare}
    (PawnPromotion newPieceType) ->
      SNPawnPromotion $
        StdNotationPawnPromotion
          { srcFile = if not (null ambiguousMoves) then Just srcSquare.file else Nothing,
            isCapture = not $ isSquareEmpty dstSquare board,
            dstSquare,
            newPiece = Piece currentPlayerColor newPieceType,
            isCheck
          }
    RegularMove ->
      SNRegularMove $
        StdNotationRegularMove
          { piece,
            srcFile =
              if any ((srcSquare.rank ==) . (.rank) . getMoveSrcSquare) ambiguousMoves
                then Just srcSquare.file
                else Nothing,
            srcRank =
              if any ((srcSquare.file ==) . (.file) . getMoveSrcSquare) ambiguousMoves
                then Just srcSquare.rank
                else Nothing,
            isCapture,
            dstSquare,
            isCheck
          }
  where
    board = getBoard game
    piece = fromJust $ getPieceAt srcSquare board
    currentPlayerColor = piece.color
    opponentColor = oppositeColor currentPlayerColor
    boardAfterMove = performLegalMoveOnBoard board move
    isCheck = isPlayerInCheck opponentColor boardAfterMove
    isCapture = length (getCapturedPieces boardAfterMove) > length (getCapturedPieces board)
    ambiguousMoves =
      filter
        ( \(Move srcSquare' dstSquare' moveType') ->
            let piece' = fromJust $ getPieceAt srcSquare' board
             in srcSquare /= srcSquare'
                  && moveType == moveType'
                  && piece == piece'
                  && dstSquare == dstSquare'
        )
        $ toList
        $ getAllLegalMoves game
