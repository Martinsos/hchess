module HChess.Core.Piece
  ( Piece (..),
    PieceType (..),
    pieceColor,
  )
where

import HChess.Core.Color (Color)

data Piece = Piece !Color !PieceType
  deriving (Eq, Show)

pieceColor :: Piece -> Color
pieceColor (Piece color _) = color

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Enum, Ord)

instance Show PieceType where
  show piece = case piece of
    Pawn -> "i"
    Knight -> "N"
    Bishop -> "B"
    Rook -> "R"
    Queen -> "Q"
    King -> "K"
