module HChess.Core.Board
  ( Board,
    Square (..),
    File (..),
    Rank (..),
    boardPieces,
    makeBoard,
    initialBoard,
    getPiece,
    isSquareEmpty,
    doesSquareContainOpponentsPiece,
  )
where

import Data.List (find)
import Data.Maybe (isNothing)
import HChess.Core.Board.File (File (..))
import HChess.Core.Board.Rank (Rank (..))
import HChess.Core.Board.Square (Square (..))
import HChess.Core.Color (Color (..))
import HChess.Core.Piece (Piece (..), PieceType (..), pieceColor)

newtype Board = Board [(Piece, Square)]
  deriving (Eq, Show)

boardPieces :: Board -> [(Piece, Square)]
boardPieces (Board pieces) = pieces

makeBoard :: [(Piece, Square)] -> Board
makeBoard = Board

initialBoard :: Board
initialBoard =
  Board $
    concat
      [ capitalPieces Black R8,
        pawns Black R7,
        pawns White R2,
        capitalPieces White R1
      ]
  where
    pawns color rank =
      (\f -> (Piece color Pawn, Square f rank)) <$> [FA .. FH]
    capitalPieces color rank =
      zip (Piece color <$> capitalPiecesOrder) ((`Square` rank) <$> [FA .. FH])
    capitalPiecesOrder =
      [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

getPiece :: Board -> Square -> Maybe Piece
getPiece (Board pieces) square = fst <$> find ((== square) . snd) pieces

isSquareEmpty :: Board -> Square -> Bool
isSquareEmpty board square = isNothing $ getPiece board square

doesSquareContainOpponentsPiece :: Color -> Board -> Square -> Bool
doesSquareContainOpponentsPiece currentPlayerColor board square =
  case getPiece board square of
    Just piece -> pieceColor piece /= currentPlayerColor
    Nothing -> False
