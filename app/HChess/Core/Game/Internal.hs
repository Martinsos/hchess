module HChess.Core.Game.Internal
  ( Game (..),
  )
where

import HChess.Core.Move (Move)

-- | First move is the latest move.
-- We made this Internal because normally we don't want a new move to be added to the Game
-- directly, instead a move to be added only by the functions that ensure validity of that move in
-- regard to the previous moves.
newtype Game = Game [Move]
  deriving (Eq)
