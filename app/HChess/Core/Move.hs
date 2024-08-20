module HChess.Core.Move
  ( Move (..),
    MoveType (..),
    getMoveDstSquare,
    performValidMoveOnBoard,
  )
where

import Data.Maybe (fromJust)
import HChess.Core.Board (Board, File (..), Square (..), getPieceAt, movePieceFromTo, putPieceAt, removeAnyPieceAt)
import HChess.Core.Piece (Piece (..), PieceType)

-- | Actual valid move that can be performed, containing some additional
-- information about its context.
-- TODO: What if we had data Move = RegularMove Square Square | EnPassant Square | KingsideCastling Color | QueensideCastling Color | PawnPromotion Square Square PieceType , so it has less chance to be invalid?
--   Additionaly, consider embedding more info into these, to make it easier to analyze them
--   standalone to some degree, without having to calculate the whole board.
--   This might be especially useful for checking conditions for game over.
-- TODO: We have notion of "simple" moves in logic that figures out moves for the pieces.
--   Should that notion also somehow be present here, in Move definition?
--   Should ReulgarMove be called SimpleMove? Or does SimpleMove also capture EnPassant?
--   Check getValidSimpleMoves.
data Move = Move !Square !Square !MoveType
  deriving (Eq, Ord)

instance Show Move where
  show (Move from to _) = show from ++ "-" ++ show to

data MoveType
  = RegularMove
  | EnPassant
  | KingsideCastling
  | QueensideCastling
  | PawnPromotion !PieceType
  deriving (Eq, Show, Ord)

getMoveDstSquare :: Move -> Square
getMoveDstSquare (Move _ dstSquare _) = dstSquare

-- | Performs a given move on the board, while assuming it is valid.
performValidMoveOnBoard :: Board -> Move -> Board
performValidMoveOnBoard board (Move src dst moveType) =
  case moveType of
    RegularMove -> movePieceFromTo' src dst board
    EnPassant ->
      movePieceFromTo' src dst
        . removeAnyPieceAt (Square (squareFile dst) (squareRank src))
        $ board
    KingsideCastling ->
      let (rookSrcSquare, rookDstSquare) = (Square FH $ squareRank src, Square FF $ squareRank src)
       in movePieceFromTo' src dst
            . movePieceFromTo' rookSrcSquare rookDstSquare
            $ board
    QueensideCastling ->
      let (rookSrcSquare, rookDstSquare) = (Square FA $ squareRank src, Square FD $ squareRank src)
       in movePieceFromTo' src dst
            . movePieceFromTo' rookSrcSquare rookDstSquare
            $ board
    PawnPromotion newPieceType ->
      let (Piece color _) = fromJust $ getPieceAt src board
       in putPieceAt dst (Piece color newPieceType)
            . removeAnyPieceAt src
            $ board
  where
    movePieceFromTo' s d b = fromJust $ movePieceFromTo s d b
