module HChess.Core.MoveOrder
  ( MoveOrder (..),
    getMoveOrderSrcSquare,
    getMoveOrderDstSquare,
    performMoveOrder,
  )
where

import Data.List (find)
import HChess.Core.Board (Square)
import HChess.Core.Game.Internal (Game (..))
import HChess.Core.LegalMoves (getLegalMoves)
import HChess.Core.Move (Move (..), MoveType (..))
import HChess.Core.Piece (PieceType)

-- | Order, by user, describing a move they would like to make.
data MoveOrder
  = MoveOrder !Square !Square
  | PawnPromotionOrder !Square !Square !PieceType

getMoveOrderSrcSquare :: MoveOrder -> Square
getMoveOrderSrcSquare = \case
  (MoveOrder sq _) -> sq
  (PawnPromotionOrder sq _ _) -> sq

getMoveOrderDstSquare :: MoveOrder -> Square
getMoveOrderDstSquare = \case
  (MoveOrder _ sq) -> sq
  (PawnPromotionOrder _ sq _) -> sq

type IllegalMoveError = String

-- | This is one of intended ways of updating the Game, since it ensures
--   that game is updated with legal move.
-- TODO: What if game is done? Do we check that here and in that case
--   don't allow performing the move? Or we don't care about that here?
performMoveOrder :: Game -> MoveOrder -> Either IllegalMoveError Game
performMoveOrder game@(Game moves) moveOrder = do
  legalMove <- moveOrderToLegalMove game moveOrder
  return $ Game $ legalMove : moves

-- | Given current state of the game and a move order, returns an actual move that
-- would match that move order, while ensuring it is legal.
moveOrderToLegalMove :: Game -> MoveOrder -> Either IllegalMoveError Move
moveOrderToLegalMove game moveOrder = do
  legalMoves <- getLegalMoves game $ getMoveOrderSrcSquare moveOrder
  case find (doesMoveOrderEqualMove moveOrder) legalMoves of
    Just legalMove -> Right legalMove
    Nothing -> Left "Illegal move."
  where
    doesMoveOrderEqualMove moveOrder' (Move srcSquare dstSquare moveType) =
      case moveOrder' of
        MoveOrder srcSquare' dstSquare' ->
          srcSquare == srcSquare'
            && dstSquare == dstSquare'
        PawnPromotionOrder srcSquare' dstSquare' newPieceType' ->
          srcSquare == srcSquare'
            && dstSquare == dstSquare'
            && moveType == PawnPromotion newPieceType'
