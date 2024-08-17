module HChess.MoveOrder
  ( MoveOrder (..),
    performMoveOrder,
  )
where

import Data.List (find)
import HChess.Board (Square)
import HChess.Game.Internal (Game (..))
import HChess.Move (Move (..), MoveType (..))
import HChess.Piece (PieceType)
import HChess.ValidMoves (getValidAndSafeMoves)

-- | Order, by user, describing a move they would like to make.
data MoveOrder = MoveOrder Square Square | PawnPromotionOrder Square Square PieceType

type InvalidMoveError = String

-- | TODO: What if game is done? Do we check that here and in that case
--   don't allow performing the move? Or we don't care about that here?
performMoveOrder :: Game -> MoveOrder -> Either InvalidMoveError Game
performMoveOrder game@(Game moves) moveOrder = do
  validMove <- moveOrderToValidMove game moveOrder
  return $ Game $ validMove : moves

-- | Given current state of the game and a move order, returns an actual move that
-- would match that move order, while ensuring it is valid.
moveOrderToValidMove :: Game -> MoveOrder -> Either InvalidMoveError Move
moveOrderToValidMove game moveOrder = do
  validMoves <- getValidAndSafeMoves game $ fst $ getMoveOrderSquares moveOrder
  case find (doesMoveOrderEqualMove moveOrder) validMoves of
    Just validMove -> Right validMove
    Nothing -> Left "Invalid move."
  where
    getMoveOrderSquares (MoveOrder src dst) = (src, dst)
    getMoveOrderSquares (PawnPromotionOrder src dst _) = (src, dst)

    doesMoveOrderEqualMove moveOrder' (Move srcSquare dstSquare moveType) =
      case moveOrder' of
        MoveOrder srcSquare' dstSquare' ->
          srcSquare == srcSquare'
            && dstSquare == dstSquare'
        PawnPromotionOrder srcSquare' dstSquare' newPieceType' ->
          srcSquare == srcSquare'
            && dstSquare == dstSquare'
            && moveType == PawnPromotion newPieceType'
