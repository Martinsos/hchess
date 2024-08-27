module HChess.Core.Piece
  ( Piece (..),
    PieceType (..),
  )
where

import HChess.Core.Color (Color)

data Piece = Piece
  { pieceColor :: !Color,
    pieceType :: !PieceType
  }
  deriving (Show, Eq, Ord)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Enum, Bounded, Ord)

instance Show PieceType where
  show piece = case piece of
    Pawn -> "i"
    Knight -> "N"
    Bishop -> "B"
    Rook -> "R"
    Queen -> "Q"
    King -> "K"
