module HChess.Core.Common
  ( startingPawnRank,
    findKing,
  )
where

import Data.List (find)
import Data.Maybe (fromJust)
import HChess.Core.Board (Board, Rank (R2), Square, boardPieces)
import HChess.Core.Board.Rank (rankToPlayerRelativeRank)
import HChess.Core.Color (Color)
import HChess.Core.Piece (Piece (..), PieceType (King))

findKing :: Color -> Board -> Square
findKing color =
  snd . fromJust . find (\(Piece c t, _) -> c == color && t == King) . boardPieces

startingPawnRank :: Color -> Rank
startingPawnRank color = rankToPlayerRelativeRank color R2
