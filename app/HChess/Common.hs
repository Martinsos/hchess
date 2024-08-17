module HChess.Common
  ( startingPawnRank,
    findKing,
  )
where

import Data.List (find)
import Data.Maybe (fromJust)
import HChess.Board (Board (..), Rank (R2), Square, rankToPlayerRelativeRank)
import HChess.Color (Color)
import HChess.Piece (Piece (..), PieceType (King))

findKing :: Color -> Board -> Square
findKing color (Board pieces) =
  snd $ fromJust $ find (\(Piece c t, _) -> c == color && t == King) pieces

startingPawnRank :: Color -> Rank
startingPawnRank color = rankToPlayerRelativeRank color R2
