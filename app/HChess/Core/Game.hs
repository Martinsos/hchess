module HChess.Core.Game
  ( Game,
    newGame,
    getMoves,
    getBoard,
    getCurrentPlayerColor,
  )
where

import Data.Foldable (foldl')
import HChess.Core.Board (Board, initialBoard)
import HChess.Core.Color (Color (..))
import HChess.Core.Game.Internal (Game (..))
import HChess.Core.Move (Move, performLegalMoveOnBoard)

newGame :: Game
newGame = Game []

-- | First move in the list is the latest move.
getMoves :: Game -> [Move]
getMoves (Game moves) = moves

getBoard :: Game -> Board
getBoard (Game moves) = foldl' performLegalMoveOnBoard initialBoard $ reverse moves

getCurrentPlayerColor :: Game -> Color
getCurrentPlayerColor (Game moves) = if even (length moves) then White else Black
