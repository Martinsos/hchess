module HChess.Core.LegalMoves.Special
  ( getPossibleSpecialMoves,
  )
where

import Control.Monad (when)
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import HChess.Core.Board
  ( Square (..),
    getPieceAt,
  )
import HChess.Core.Board.Square
  ( squareBackward,
    squareForward,
    squareLeft,
    squareRight,
  )
import HChess.Core.Color (oppositeColor)
import HChess.Core.Common (startingPawnRank)
import HChess.Core.Game (Game, getBoard, getCurrentPlayerColor, getMoves)
import HChess.Core.Move (Move (..), MoveType (..))
import HChess.Core.Piece (Piece (..), PieceType (..))
import HChess.Utils (maybeToEither)

-- | Special moves are castling and enpassant. They are special because they require history of the game, not just current board state.
-- Also, interesting and important -> none of them can attack enemy king.
getPossibleSpecialMoves :: Game -> Square -> Either String (S.Set Move)
getPossibleSpecialMoves game srcSquare = do
  -- TODO: This is duplicated in getPossibleSimpleMoves.
  (Piece srcPieceColor srcPieceType) <-
    maybeToEither "No piece at specified location" $ getPieceAt srcSquare board

  -- TODO: This is duplicated in getPossibleSimpleMoves.
  when (srcPieceColor /= currentPlayerColor) $
    Left "Can't move oponnent's piece"

  -- TODO: Extract this into special getPossibleSpecialMoves function?
  return $ case srcPieceType of
    Pawn -> pawnPossibleSpecialMoves
    King -> kingPossibleSpecialMoves
    _ -> S.empty
  where
    (board, currentPlayerColor) = (getBoard game, getCurrentPlayerColor game)

    pawnPossibleSpecialMoves =
      (S.fromList . catMaybes)
        [ squareFwd srcSquare >>= squareRight >>= makeAnEnPassantMove,
          squareFwd srcSquare >>= squareLeft >>= makeAnEnPassantMove
        ]
      where
        -- TODO: Duplication.
        squareFwd = squareForward currentPlayerColor

        makeAnEnPassantMove :: Square -> Maybe Move
        makeAnEnPassantMove dstSquare
          | isMoveEnPassant game dstSquare = return $ mkMove EnPassant dstSquare
          | otherwise = Nothing

        -- TODO: Duplication.
        mkMove :: MoveType -> Square -> Move
        mkMove moveType dstSquare = Move srcSquare dstSquare moveType

    kingPossibleSpecialMoves =
      -- TODO:
      --  1. Check that king was never moved.
      --  2. Check that rook has never moved.
      --  3. Check that square between the kind and rook are vacant.
      --  4. Neither king, rook, or any square in between them is under attack.
      --  We need to check this for both kingside castling and for queenside castling.
      --  To check if king has never moved, it is best to just check if there was ever a move that had its initial square as src.
      --  To check if rook has never moved, we can do this same check, although it might have in theory been moved by castling,
      --    but in that case the check with "did king move" will fail first anyway.
      --  Checking that squares in between are vacant should be trivial.
      --  Checking that none of the squares is under attack: I should take a look at `isPlayerInCheck` function,
      --  refactor it into more general `isSquareUnderAttack` function, and then use that function here plus
      --  also redefine isPlayerInCheck via it.
      error "TODO: castling"

-- | For a given destination square, determine if landing a pawn on that square would be en passant
-- move. We assume that move is possible, in a sense that there is such pawn, owned by the player that
-- currently has turn, that can be moved to this square.
isMoveEnPassant :: Game -> Square -> Bool
isMoveEnPassant game dstSquare = case getMoves game of
  [] -> False
  ((Move lastMoveSrcSquare@(Square _ lastMoveSrcSquareRank) lastMoveDstSquare _) : _) ->
    let lastMoveWasOpponentMovingPawnForTwoSquares =
          getPieceAt lastMoveSrcSquare board == Just (Piece opponentColor Pawn)
            && lastMoveSrcSquareRank == startingPawnRank opponentColor
            && Just lastMoveDstSquare == (squareForward opponentColor lastMoveSrcSquare >>= squareForward opponentColor)
        currentMoveIsSquareBehindLastMoveDstSquare =
          squareBackward opponentColor lastMoveDstSquare == Just dstSquare
     in lastMoveWasOpponentMovingPawnForTwoSquares
          && currentMoveIsSquareBehindLastMoveDstSquare
  where
    currentPlayerColor = getCurrentPlayerColor game
    opponentColor = oppositeColor currentPlayerColor
    board = getBoard game
