module HChess.Core.PossibleMoves.Simple
  ( getPossibleSimpleMoves,
  )
where

import Control.Monad (when, (<=<))
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import HChess.Core.Board
  ( Board,
    Rank (..),
    Square (..),
    doesSquareContainOpponentsPiece,
    getPieceAt,
    isSquareEmpty,
  )
import HChess.Core.Board.Movement
  ( getDiagonallyAccessibleSquares,
    getPerpendicularlyAccessibleSquares,
  )
import HChess.Core.Board.Rank (rankToPlayerRelativeRank)
import HChess.Core.Board.Square
  ( squareDown,
    squareForward,
    squareLeft,
    squareRight,
    squareUp,
  )
import HChess.Core.Color (Color (..), oppositeColor)
import HChess.Core.Common (findKing, startingPawnRank)
import HChess.Core.Move (Move (..), MoveType (..))
import HChess.Core.Piece (Piece (..), PieceType (..))
import HChess.Utils (maybeToEither, validate)

-- | Returns all "simple" moves: moves that don't require history of the game but only current state
-- of the board. That is all the moves except for castling and en passant. Also, it doesn't check if
-- a move exposes its own king to a check, therefore returning all possible moves, not just legal
-- ones.
getPossibleSimpleMoves :: Color -> Board -> Square -> Either String (S.Set Move)
getPossibleSimpleMoves currentPlayerColor board src@(Square _ srcRank) = do
  (Piece srcPieceColor srcPieceType) <-
    maybeToEither "No piece at specified location" $ getPieceAt src board

  when (srcPieceColor /= currentPlayerColor) $
    Left "Can't move oponnent's piece"

  let possibleMoves = case srcPieceType of
        Pawn -> pawnPossibleMoves
        Knight -> knightPossibleMoves
        Bishop -> bishopPossibleMoves
        Rook -> rookPossibleMoves
        Queen -> queenPossibleMoves
        King -> kingPossibleMoves
  return possibleMoves
  where
    mkMove :: MoveType -> Square -> Move
    mkMove moveType dstSquare = Move src dstSquare moveType

    bishopPossibleMoves :: S.Set Move
    bishopPossibleMoves =
      mkMove RegularMove
        `S.map` getDiagonallyAccessibleSquares currentPlayerColor board src

    rookPossibleMoves :: S.Set Move
    rookPossibleMoves =
      mkMove RegularMove
        `S.map` getPerpendicularlyAccessibleSquares currentPlayerColor board src

    queenPossibleMoves :: S.Set Move
    queenPossibleMoves =
      mkMove RegularMove
        `S.map` mconcat
          [ getDiagonallyAccessibleSquares currentPlayerColor board src,
            getPerpendicularlyAccessibleSquares currentPlayerColor board src
          ]

    kingPossibleMoves :: S.Set Move
    kingPossibleMoves =
      filterOutMovesTooCloseToOpponentsKing $ getKingsMoves src
      where
        getKingsMoves kingsSrcSquare =
          (S.fromList . (mkMove RegularMove <$>) . mapMaybe ($ kingsSrcSquare))
            [ squareUp,
              squareUp <=< squareLeft,
              squareUp <=< squareRight,
              squareLeft,
              squareRight,
              squareDown,
              squareDown <=< squareLeft,
              squareDown <=< squareRight
            ]
        opponentsKingSquare = findKing (oppositeColor currentPlayerColor) board
        filterOutMovesTooCloseToOpponentsKing kingsMoves =
          kingsMoves `S.difference` getKingsMoves opponentsKingSquare

    knightPossibleMoves :: S.Set Move
    knightPossibleMoves =
      (S.fromList . (mkMove RegularMove <$>) . mapMaybe ($ src))
        [ squareUp <=< squareUp <=< squareRight,
          squareUp <=< squareUp <=< squareLeft,
          squareUp <=< squareRight <=< squareRight,
          squareUp <=< squareLeft <=< squareLeft,
          squareDown <=< squareDown <=< squareRight,
          squareDown <=< squareDown <=< squareLeft,
          squareDown <=< squareRight <=< squareRight,
          squareDown <=< squareLeft <=< squareLeft
        ]

    pawnPossibleMoves :: S.Set Move
    pawnPossibleMoves =
      (S.fromList . detectAndLabelPawnPromotionMoves . catMaybes)
        [ moveOneSquareForward,
          moveTwoSquaresForward,
          attackForwardRight,
          attackForwardLeft
        ]
      where
        squareFwd :: Square -> Maybe Square
        squareFwd = squareForward currentPlayerColor

        moveOneSquareForward :: Maybe Move
        moveOneSquareForward =
          mkMove RegularMove
            <$> (squareFwd src >>= validate (`isSquareEmpty` board))

        moveTwoSquaresForward :: Maybe Move
        moveTwoSquaresForward = do
          if srcRank /= startingPawnRank currentPlayerColor
            then Nothing
            else
              mkMove RegularMove
                <$> ( squareFwd src
                        >>= validate (`isSquareEmpty` board)
                        >>= squareFwd
                        >>= validate (`isSquareEmpty` board)
                    )

        attackForwardRight :: Maybe Move
        attackForwardRight = squareFwd src >>= squareRight >>= registerAsAttack

        attackForwardLeft :: Maybe Move
        attackForwardLeft = squareFwd src >>= squareLeft >>= registerAsAttack

        registerAsAttack :: Square -> Maybe Move
        registerAsAttack dstSquare
          | doesSquareContainOpponentsPiece currentPlayerColor dstSquare board =
              return $ mkMove RegularMove dstSquare
          | otherwise =
              Nothing

        detectAndLabelPawnPromotionMoves :: [Move] -> [Move]
        detectAndLabelPawnPromotionMoves = concatMap detectAndLabelPawnPromotionMove
          where
            detectAndLabelPawnPromotionMove :: Move -> [Move]
            detectAndLabelPawnPromotionMove (Move srcSquare dstSquare@(Square _ dstRank) _)
              | dstRank == rankToPlayerRelativeRank currentPlayerColor R8 =
                  Move srcSquare dstSquare . PawnPromotion <$> [Knight, Bishop, Rook, Queen]
            detectAndLabelPawnPromotionMove move = [move]
