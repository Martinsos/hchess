module HChess.Game
  ( Game,
    newGame,
    getMoves,
    getBoard,
    getCurrentPlayerColor,
  )
where

import Data.Foldable (foldl')
import HChess.Board (Board, initialBoard)
import HChess.Color (Color (..))
import HChess.Game.Internal (Game (..))
import HChess.Move (Move, performValidMoveOnBoard)

getCurrentPlayerColor :: Game -> Color
getCurrentPlayerColor (Game moves) = if even (length moves) then White else Black

newGame :: Game
newGame = Game []

getMoves :: Game -> [Move]
getMoves (Game moves) = moves

getBoard :: Game -> Board
getBoard (Game moves) = foldl' performValidMoveOnBoard initialBoard $ reverse moves
