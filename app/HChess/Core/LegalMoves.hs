module HChess.Core.LegalMoves
  ( getLegalMoves,
  )
where

import qualified Data.Set as S
import HChess.Core.Board.Square (Square (..))
import HChess.Core.Check (isPlayerInCheck)
import HChess.Core.Game (getBoard, getCurrentPlayerColor)
import HChess.Core.Game.Internal (Game (..))
import HChess.Core.LegalMoves.Simple (getPossibleSimpleMoves)
import HChess.Core.LegalMoves.Special (getPossibleSpecialMoves)
import HChess.Core.Move (Move (..), performLegalMoveOnBoard)

-- TODO: Can I merge getPossibleSimpleMoves and getPossibleSpecialMoves into one function? If so, should I?

type PossibleMovesInquiryError = String

-- | Returns all legal moves that can be done in the specified game with the piece at the specified
-- square.
getLegalMoves :: Game -> Square -> Either PossibleMovesInquiryError (S.Set Move)
getLegalMoves game srcSquare = do
  possibleMoves <- getPossibleMoves game srcSquare
  return $ S.filter (not . doesMovePutOwnKingInCheck) possibleMoves
  where
    (board, currentPlayerColor) = (getBoard game, getCurrentPlayerColor game)

    doesMovePutOwnKingInCheck :: Move -> Bool
    doesMovePutOwnKingInCheck move =
      isPlayerInCheck currentPlayerColor $ performLegalMoveOnBoard board move

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
