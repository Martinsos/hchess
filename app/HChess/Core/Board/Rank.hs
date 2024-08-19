module HChess.Core.Board.Rank
  ( Rank (..),
    rankToPlayerRelativeRank,
  )
where

import HChess.Core.Color (Color (..))

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving (Eq, Ord, Enum, Bounded)

instance Show Rank where
  show = show . (+ 1) . fromEnum

rankToPlayerRelativeRank :: Color -> Rank -> Rank
rankToPlayerRelativeRank color rank =
  case color of
    White -> rank
    Black -> toEnum (7 - fromEnum rank)
