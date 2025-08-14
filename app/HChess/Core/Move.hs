module HChess.Core.Move
  ( Move (..),
    MoveType (..),
    getMoveSrcSquare,
    getMoveDstSquare,
    performLegalMoveOnBoard,
  )
where

import Data.Maybe (fromJust)
import HChess.Core.Board
  ( Board,
    File (..),
    Square (..),
    getPieceAt,
    movePieceFromTo,
    putPieceAt,
    removeAnyPieceAt,
  )
import HChess.Core.Piece (Piece (..), PieceType)

-- TODO: Consider using LiquidHaskell for stronger compile time checks, for example to check
--   that Move applied to a Game indeed was created via function that generates valid
--   moves for that specific game. Something like {-@ ... g:Game -> { r: [Move] | isValidMove r g } ... -> this is wrong code I wrote here but it is a very rough idea.

-- | TODO: What if we had data Move = RegularMove Square Square | EnPassant Square | KingsideCastling Color | QueensideCastling Color | PawnPromotion Square Square PieceType , so it has less chance to be invalid?
--   Additionaly, consider embedding more info into these, to make it easier to analyze them
--   standalone to some degree, without having to calculate the whole board.
--   This might be especially useful for checking conditions for game over.
-- TODO: We have notion of "simple" moves in logic that figures out moves for the pieces.
--   Should that notion also somehow be present here, in Move definition?
--   Should ReulgarMove be called SimpleMove? Or does SimpleMove also capture EnPassant?
--   Check getPossibleSimpleMoves.
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

getMoveSrcSquare :: Move -> Square
getMoveSrcSquare (Move srcSquare _ _) = srcSquare

getMoveDstSquare :: Move -> Square
getMoveDstSquare (Move _ dstSquare _) = dstSquare

-- | Performs a given move on the board, while assuming it is legal.
performLegalMoveOnBoard :: Board -> Move -> Board
performLegalMoveOnBoard board (Move src dst moveType) =
  case moveType of
    RegularMove -> movePieceFromTo' src dst board
    EnPassant ->
      movePieceFromTo' src dst
        . removeAnyPieceAt (Square dst.file src.rank)
        $ board
    KingsideCastling ->
      let (rookSrcSquare, rookDstSquare) = (Square FH src.rank, Square FF src.rank)
       in movePieceFromTo' src dst
            . movePieceFromTo' rookSrcSquare rookDstSquare
            $ board
    QueensideCastling ->
      let (rookSrcSquare, rookDstSquare) = (Square FA src.rank, Square FD src.rank)
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
