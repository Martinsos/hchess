module HChess.Core.Board.Square
  ( Square (..),
    squareColor,
    squareLeft,
    squareRight,
    squareUp,
    squareDown,
    squareForward,
    squareBackward,
  )
where

import HChess.Core.Board.File (File (..))
import HChess.Core.Board.Rank (Rank (..))
import HChess.Core.Color (Color (..))
import HChess.Utils (safePred, safeSucc)

data Square = Square
  { squareFile :: !File,
    squareRank :: !Rank
  }
  deriving (Eq, Ord)

instance Show Square where
  show (Square f r) = show f <> show r

squareColor :: Square -> Color
squareColor (Square file rank) =
  if even $ fromEnum file + fromEnum rank
    then Black
    else White

squareUp :: Square -> Maybe Square
squareUp (Square f r) = (f `Square`) <$> safeSucc r

squareDown :: Square -> Maybe Square
squareDown (Square f r) = (f `Square`) <$> safePred r

squareRight :: Square -> Maybe Square
squareRight (Square f r) = (`Square` r) <$> safeSucc f

squareLeft :: Square -> Maybe Square
squareLeft (Square f r) = (`Square` r) <$> safePred f

squareForward :: Color -> Square -> Maybe Square
squareForward White = squareUp
squareForward Black = squareDown

squareBackward :: Color -> Square -> Maybe Square
squareBackward White = squareDown
squareBackward Black = squareUp
