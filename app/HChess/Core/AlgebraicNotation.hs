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
    Square (squareRank),
    getCapturedPieces,
    getPieceAt,
    isSquareEmpty,
    squareFile,
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
import HChess.Core.Piece (Piece (Piece, pieceColor, pieceType), PieceType (Pawn), pieceToUnicode)

data StdNotationMove
  = SNRegularMove !StdNotationRegularMove
  | SNEnPassant !StdNotationEnPassant
  | SNPawnPromotion !StdNotationPawnPromotion
  | SNKingsideCastling
  | SNQueensideCastling

-- TODO: Try using a better solution for the records here. Something more modern ({-# LANGUAGE RecordWildCards #-} {-# LANGUAGE NamedFieldPuns #-}) or split them into files.

data StdNotationRegularMove = StdNotationRegularMove
  { snrmPiece :: !Piece,
    snrmSrcFile :: !(Maybe File),
    snrmSrcRank :: !(Maybe Rank),
    snrmIsCapture :: !Bool,
    snrmDstSquare :: !Square,
    snrmIsCheck :: !Bool
  }

data StdNotationEnPassant = StdNotationEnPassant
  { snepSrcFile :: !File,
    snepDstSquare :: !Square
  }

data StdNotationPawnPromotion = StdNotationPawnPromotion
  { snppSrcFile :: !(Maybe File),
    snppIsCapture :: !Bool,
    snppDstSquare :: !Square,
    snppNewPiece :: !Piece,
    snppIsCheck :: !Bool
  }

instance Show StdNotationMove where
  show SNKingsideCastling = "O-O"
  show SNQueensideCastling = "O-O-O"
  show (SNEnPassant move) =
    show (snepSrcFile move) <> captureSymbol <> show (snepDstSquare move) <> " e.p."
  show (SNPawnPromotion move) =
    maybe "" show (snppSrcFile move)
      <> (if snppIsCapture move then captureSymbol else "")
      <> show (snppDstSquare move)
      <> "="
      <> pieceToUnicode (snppNewPiece move)
      <> (if snppIsCheck move then checkSymbol else "")
  show (SNRegularMove move) =
    ( if pieceType (snrmPiece move) == Pawn
        then ""
        else pieceToUnicode (snrmPiece move)
    )
      <> maybe "" show (snrmSrcFile move)
      <> maybe "" show (snrmSrcRank move)
      <> (if snrmIsCapture move then captureSymbol else "")
      <> show (snrmDstSquare move)
      <> (if snrmIsCheck move then checkSymbol else "")

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
    EnPassant ->
      SNEnPassant $
        StdNotationEnPassant
          { snepSrcFile = squareFile srcSquare,
            snepDstSquare = dstSquare
          }
    (PawnPromotion newPieceType) ->
      SNPawnPromotion $
        StdNotationPawnPromotion
          { snppSrcFile =
              if not (null ambiguousMoves)
                then Just (squareFile srcSquare)
                else Nothing,
            snppIsCapture = not $ isSquareEmpty dstSquare board,
            snppDstSquare = dstSquare,
            snppNewPiece = Piece currentPlayerColor newPieceType,
            snppIsCheck = isCheck
          }
    RegularMove ->
      SNRegularMove $
        StdNotationRegularMove
          { snrmPiece = piece,
            snrmSrcFile =
              if any ((squareRank srcSquare ==) . squareRank . getMoveSrcSquare) ambiguousMoves
                then Just (squareFile srcSquare)
                else Nothing,
            snrmSrcRank =
              if any ((squareFile srcSquare ==) . squareFile . getMoveSrcSquare) ambiguousMoves
                then Just (squareRank srcSquare)
                else Nothing,
            snrmIsCapture = isCapture,
            snrmDstSquare = dstSquare,
            snrmIsCheck = isCheck
          }
  where
    board = getBoard game
    piece = fromJust $ getPieceAt srcSquare board
    currentPlayerColor = pieceColor piece
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
        $ toList $ getAllLegalMoves game
