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
import HChess.Core.Move (Move (..), MoveType (..))
import HChess.Core.Piece (PieceType)
import HChess.Core.ValidMoves (getValidAndSafeMoves)

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

type InvalidMoveError = String

-- | This is one of intended ways of updating the Game, since it ensures
--   that game is updated with valid move.
-- TODO: What if game is done? Do we check that here and in that case
--   don't allow performing the move? Or we don't care about that here?
performMoveOrder :: Game -> MoveOrder -> Either InvalidMoveError Game
performMoveOrder game@(Game moves) moveOrder = do
  validMove <- moveOrderToValidMove game moveOrder
  return $ Game $ validMove : moves

-- | Given current state of the game and a move order, returns an actual move that
-- would match that move order, while ensuring it is valid.
moveOrderToValidMove :: Game -> MoveOrder -> Either InvalidMoveError Move
moveOrderToValidMove game moveOrder = do
  validMoves <- getValidAndSafeMoves game $ getMoveOrderSrcSquare moveOrder
  case find (doesMoveOrderEqualMove moveOrder) validMoves of
    Just validMove -> Right validMove
    Nothing -> Left "Invalid move."
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
