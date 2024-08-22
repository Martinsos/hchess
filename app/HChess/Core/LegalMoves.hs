module HChess.Core.LegalMoves
  ( getLegalMoves,
  )
where

import qualified Data.Set as S
import HChess.Core.Board.Square (Square (..))
import HChess.Core.Check (isPlayerInCheck)
import HChess.Core.Game (getBoard, getCurrentPlayerColor)
import HChess.Core.Game.Internal (Game (..))
import HChess.Core.Move (Move (..), performLegalMoveOnBoard)
import HChess.Core.PossibleMoves (getPossibleMoves)

type LegalMovesInquiryError = String

-- | Returns all legal moves that can be done in the specified game with the piece at the specified
-- square.
getLegalMoves :: Game -> Square -> Either LegalMovesInquiryError (S.Set Move)
getLegalMoves game srcSquare = do
  possibleMoves <- getPossibleMoves game srcSquare
  return $ S.filter (not . doesMovePutOwnKingInCheck) possibleMoves
  where
    (board, currentPlayerColor) = (getBoard game, getCurrentPlayerColor game)

    doesMovePutOwnKingInCheck :: Move -> Bool
    doesMovePutOwnKingInCheck move =
      isPlayerInCheck currentPlayerColor $ performLegalMoveOnBoard board move
