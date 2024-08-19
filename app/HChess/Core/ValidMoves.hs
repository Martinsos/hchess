module HChess.Core.ValidMoves
  ( getValidAndSafeMoves,
  )
where

import Control.Monad (when)
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import HChess.Core.Board
  ( Square (..),
    getPiece,
    squareBackward,
    squareForward,
    squareLeft,
    squareRight,
  )
import HChess.Core.Check (isPlayerInCheck)
import HChess.Core.Color (oppositeColor)
import HChess.Core.Common (startingPawnRank)
import HChess.Core.Game (getBoard, getCurrentPlayerColor, getMoves)
import HChess.Core.Game.Internal (Game (..))
import HChess.Core.Move (Move (..), MoveType (..), performValidMoveOnBoard)
import HChess.Core.Piece (Piece (..), PieceType (..))
import HChess.Core.ValidMoves.Simple (getValidSimpleMoves)
import HChess.Utils (maybeToEither)

-- TODO: This function is huge, take it out into separate module and move complex functions in @where@ to standalone functions.

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

-- NOTE: This returns all moves including for castling and en passant. It doesn't check if a move exposes its own king to a check.
getValidMoves :: Game -> Square -> Either ValidMovesInquiryError (S.Set Move)
getValidMoves game srcSquare = do
  -- TODO: This is duplicated in getValidSimpleMoves.
  (Piece srcPieceColor srcPieceType) <- maybeToEither "No piece at specified location" $ getPiece board srcSquare
  when (srcPieceColor /= currentPlayerColor) $ Left "Can't move oponnent's piece"

  simpleMoves <- getValidSimpleMoves currentPlayerColor board srcSquare

  -- NOTE: special moves are castling and enpassant. They are special because they require history of the game, not just current board state.
  -- Also, interesting and important -> none of them can attack enemy king.
  -- TODO: Extract this into special getValidSpecialMoves function?
  let specialMoves = case srcPieceType of
        Pawn -> pawnValidSpecialMoves
        King -> kingValidSpecialMoves
        _ -> S.empty

  return $ simpleMoves `S.union` specialMoves
  where
    (board, currentPlayerColor) = (getBoard game, getCurrentPlayerColor game)

    pawnValidSpecialMoves =
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

    kingValidSpecialMoves =
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

-- | For a given destination square, determine if landing a pawn on that square would be en passant move.
-- We assume that move is valid, in a sense that there is such pawn, owned by the player that currently has turn,
-- that can be moved to this square.
-- TODO: This feels pretty much like a helper function, maybe move it somewhere to make it less global?
isMoveEnPassant :: Game -> Square -> Bool
isMoveEnPassant game dstSquare = case getMoves game of
  [] -> False
  ((Move lastMoveSrcSquare@(Square _ lastMoveSrcSquareRank) lastMoveDstSquare _) : _) ->
    let lastMoveWasOpponentMovingPawnForTwoSquares =
          getPiece board lastMoveSrcSquare == Just (Piece opponentColor Pawn)
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
