module Main where

import Data.Char (chr, ord)

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Game = Game [Move]
  deriving (Eq)

data Color = White | Black
  deriving (Eq, Show)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Enum, Ord)

data Piece = Piece Color PieceType
  deriving (Eq, Show)

data Move = Move Square Square
  deriving (Eq)

data Square = Square File Rank
  deriving (Eq)

data File = FA | FB | FC | FD | FE | FF | FG | FH
  deriving (Eq, Ord, Enum)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving (Eq, Ord, Enum)

data Board = Board [(Piece, Square)]
  deriving (Eq, Show)

instance Show Move where
  show (Move from to) = show from ++ "-" ++ show to

instance Show Square where
  show (Square f r) = show f ++ show r

instance Show File where
  show = show . chr . (ord 'a' +) . fromEnum

instance Show Rank where
  show = show . (+ 1) . fromEnum

instance Show PieceType where
  show piece = case piece of
    Pawn -> "i"
    Knight -> "N"
    Bishop -> "B"
    Rook -> "R"
    Queen -> "Q"
    King -> "K"

type InvalidMoveMsg = String

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
    pawns color rank = (\f -> (Piece color Pawn, Square f rank)) <$> [FA .. FH]
    capitalPieces color rank = zip (Piece color <$> capitalPiecesOrder) ((\f -> Square f rank) <$> [FA .. FH])
    capitalPiecesOrder = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

performMove :: Board -> Move -> Either InvalidMoveMsg Board
performMove = undefined
