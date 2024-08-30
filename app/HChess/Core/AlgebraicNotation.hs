module HChess.Core.AlgebraicNotation
  (
  )
where

import HChess.Core.Board (Board, File, Rank, Square)
import HChess.Core.Game (Game)
import HChess.Core.Move (Move)
import HChess.Core.Piece (Piece)

data StandardNotationMove
  = SNRegularMove !StandardNotationRegularMove
  | SNEnPassant !StandardNotationEnPassant
  | SNPawnPromotion !StandardNotationPawnPromotion
  | SNKingsideCastling
  | SNQueensideCastling

data StandardNotationRegularMove = StandardNotationRegularMove
  { snrmPiece :: !Piece,
    snrmSrcFile :: !(Maybe File),
    snrmSrcRank :: !(Maybe Rank),
    snrmIsCapture :: !Bool,
    snrmDstSquare :: !Square,
    snrmIsCheck :: !Bool
  }

data StandardNotationEnPassant = StandardNotationEnPassant
  { snepSrcFile :: !File,
    snepDstSquare :: !Square
  }

data StandardNotationPawnPromotion = StandardNotationPawnPromotion
  { snppSrcFile :: !(Maybe File),
    snppIsCapture :: !Bool,
    snppDstSquare :: !Square,
    snppNewPiece :: !Piece,
    snppIsCheck :: !Bool
  }

instance Show StandardNotationMove where
  show SNKingsideCastling = "O-O"
  show SNQueensideCastling = "O-O-O"
  show (SNEnPassant move) =
    show (snepSrcFile move) <> captureSymbol <> show (snepDstSquare move) <> " e.p."
  show (SNPawnPromotion move) =
    show (snppSrcFile move)
      <> (if snppIsCapture move then captureSymbol else "")
      <> show (snppDstSquare move)
      <> "="
      <> show (snppNewPiece move)
      <> (if snppIsCheck move then checkSymbol else "")
  show (SNRegularMove move) =
    showPiece (snrmPiece move)
      <> maybe "" show (snrmSrcFile move)
      <> maybe "" show (snrmSrcRank move)
      <> (if snrmIsCapture move then captureSymbol else "")
      <> show (snrmDstSquare move)
      <> (if snrmIsCheck move then checkSymbol else "")

captureSymbol :: String
captureSymbol = "x"

checkSymbol :: String
checkSymbol = "+"

showPiece :: Piece -> String
showPiece = error "TODO"

getStandardNotationForGame :: Game -> [StandardNotationMove]
getStandardNotationForGame = error "TODO"

getStandardNotationForLegalMove :: Move -> Board -> StandardNotationMove
getStandardNotationForLegalMove = error "TODO"
