module HChess.Core.Piece
  ( Piece (..),
    PieceType (..),
    pieceToUnicode,
  )
where

import HChess.Core.Color (Color (..))

data Piece = Piece
  { color :: !Color,
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

pieceToUnicode :: Piece -> String
pieceToUnicode (Piece Black pType) = case pType of
  King -> "\x2654"
  Queen -> "\x2655"
  Rook -> "\x2656"
  Bishop -> "\x2657"
  Knight -> "\x2658"
  Pawn -> "\x2659"
pieceToUnicode (Piece White pType) = case pType of
  King -> "\x265A"
  Queen -> "\x265B"
  Rook -> "\x265C"
  Bishop -> "\x265D"
  Knight -> "\x265E"
  Pawn -> "\x265F"
