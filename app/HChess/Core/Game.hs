module HChess.Core.Game
  ( Game,
    newGame,
    getMoves,
    getBoard,
    getCurrentPlayerColor,
    oneMoveBack,
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

-- | TODO: Here I assume game always starts with the normal, fresh new chess board.
-- But, for the sake of chess puzzles and similar, it might make sense to sometimes have it start
-- from some other starting point. We could support this by passing in the initial board
-- when creating a new game, and not assuming the initial board.
getBoard :: Game -> Board
getBoard (Game moves) = foldl' performLegalMoveOnBoard initialBoard $ reverse moves

getCurrentPlayerColor :: Game -> Color
getCurrentPlayerColor (Game moves) = if even (length moves) then White else Black

oneMoveBack :: Game -> Game
oneMoveBack (Game []) = Game []
oneMoveBack (Game (_ : moves)) = Game moves
