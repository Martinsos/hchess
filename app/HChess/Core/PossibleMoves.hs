module HChess.Core.PossibleMoves
  ( getPossibleMoves,
  )
where

import qualified Data.Set as S
import HChess.Core.Board.Square (Square (..))
import HChess.Core.Game (getBoard, getCurrentPlayerColor)
import HChess.Core.Game.Internal (Game (..))
import HChess.Core.Move (Move (..))
import HChess.Core.PossibleMoves.Simple (getPossibleSimpleMoves)
import HChess.Core.PossibleMoves.Special (getPossibleSpecialMoves)

type PossibleMovesInquiryError = String

-- | Returns all moves that can be done in the specified game with the piece at the specified
-- square, while not checking if a move exposes its own king to a check (therefore name "possible" and
-- not "legal").
getPossibleMoves :: Game -> Square -> Either PossibleMovesInquiryError (S.Set Move)
getPossibleMoves game srcSquare = do
  simpleMoves <- getPossibleSimpleMoves currentPlayerColor board srcSquare
  specialMoves <- getPossibleSpecialMoves game srcSquare
  return $ simpleMoves `S.union` specialMoves
  where
    (board, currentPlayerColor) = (getBoard game, getCurrentPlayerColor game)
