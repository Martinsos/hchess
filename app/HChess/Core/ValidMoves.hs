module HChess.Core.ValidMoves
  ( getValidAndSafeMoves,
  )
where

import qualified Data.Set as S
import HChess.Core.Board.Square (Square (..))
import HChess.Core.Check (isPlayerInCheck)
import HChess.Core.Game (getBoard, getCurrentPlayerColor)
import HChess.Core.Game.Internal (Game (..))
import HChess.Core.Move (Move (..), performValidMoveOnBoard)
import HChess.Core.ValidMoves.Simple (getValidSimpleMoves)
import HChess.Core.ValidMoves.Special (getValidSpecialMoves)

-- TODO: Rename "valid" to "possible" and "validAndSafe" to "valid"? What I am trying to do is find a name
--   for moves that are valid but might expose the king to check, and then also find a name for moves
--   that are valid but also don't expose the king to a check.
--   Or maybe I should drop the idea of having two names for this stuff and just go with "valid" moves,
--   which are moves that are both ok and don't put king in danger? getValidAndSafeMoves doesn't do much
--   at the moment anyway, I could just move its logic into getValidMoves? I can define helper function
--   that filter outs moves that endanger king, and just call that in the getValidMoves and there we go.
--   Ah but that is an issue because I have helper functions that look for valid but unsafe moves so how
--   do I call them? Maybe this will be a bit easier once I extract it into a separate module.
--   Maybe "valid" vs "potentially valid"?
--   Yeah, I should probably just call "validAndSafe" move "valid" moves, drop the concept of unsafe ones,
--   and push logic from `getValidAndSafeMoves` into `getValidMoves`.

-- TODO: Can I merge getValidSimpleMoves and getValidSpecialMoves into one function? If so, should I?

type ValidMovesInquiryError = String

getValidAndSafeMoves :: Game -> Square -> Either ValidMovesInquiryError (S.Set Move)
getValidAndSafeMoves game srcSquare = do
  validMoves <- getValidMoves game srcSquare
  return $ S.filter (not . doesMovePutOwnKingInCheck) validMoves
  where
    (board, currentPlayerColor) = (getBoard game, getCurrentPlayerColor game)

    doesMovePutOwnKingInCheck :: Move -> Bool
    doesMovePutOwnKingInCheck move =
      isPlayerInCheck currentPlayerColor $ performValidMoveOnBoard board move

-- NOTE: This returns all moves including castlings and en passant. It doesn't check if a move exposes its own king to a check.
getValidMoves :: Game -> Square -> Either ValidMovesInquiryError (S.Set Move)
getValidMoves game srcSquare = do
  simpleMoves <- getValidSimpleMoves currentPlayerColor board srcSquare
  specialMoves <- getValidSpecialMoves game srcSquare
  return $ simpleMoves `S.union` specialMoves
  where
    (board, currentPlayerColor) = (getBoard game, getCurrentPlayerColor game)
