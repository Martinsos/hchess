module HChess.Core.Move
  ( Move (..),
    MoveType (..),
    getMoveDstSquare,
    performValidMoveOnBoard,
  )
where

import Data.List (find)
import Data.Maybe (fromJust)
import HChess.Core.Board (Board (..), File (..), Square (..))
import HChess.Core.Piece (Piece (..), PieceType)

-- | Actual valid move that can be performed, containing some additional
-- information about its context.
-- TODO: What if we had data Move = RegularMove Square Square | EnPassant Square | KingsideCastling | QueensideCastling | PawnPromotion Square Square PieceType , so it has less chance to be invalid?
--   Additionaly, consider embedding more info into these, to make it easier to analyze them
--   standalone to some degree, without having to calculate the whole board.
--   This might be especially useful for checking conditions for game over.
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
    RegularMove -> movePieceFromTo src dst . removeAnyPieceAt dst $ board
    EnPassant -> movePieceFromTo src dst . removeAnyPieceAt (Square (squareFile dst) (squareRank src)) $ board
    KingsideCastling ->
      let (rookSrcSquare, rookDstSquare) = (Square FH $ squareRank src, Square FF $ squareRank src)
       in movePieceFromTo src dst . movePieceFromTo rookSrcSquare rookDstSquare $ board
    QueensideCastling ->
      let (rookSrcSquare, rookDstSquare) = (Square FA $ squareRank src, Square FD $ squareRank src)
       in movePieceFromTo src dst . movePieceFromTo rookSrcSquare rookDstSquare $ board
    PawnPromotion newPieceType ->
      let (Piece color _) = fromJust $ getPieceAt src board
       in putPieceAt dst (Piece color newPieceType) . removeAnyPieceAt src $ board
  where
    movePieceFromTo :: Square -> Square -> Board -> Board
    movePieceFromTo src' dst' (Board pieces) =
      Board $ (fmap . fmap) (\sq -> if sq == src' then dst' else sq) pieces

    removeAnyPieceAt :: Square -> Board -> Board
    removeAnyPieceAt square (Board pieces) = Board $ filter ((/= square) . snd) pieces

    putPieceAt :: Square -> Piece -> Board -> Board
    putPieceAt sq piece (Board pieces) = Board $ (piece, sq) : pieces

    getPieceAt :: Square -> Board -> Maybe Piece
    getPieceAt sq (Board pieces) = fst <$> find ((== sq) . snd) pieces
