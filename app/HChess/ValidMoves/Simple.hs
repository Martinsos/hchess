module HChess.ValidMoves.Simple
  ( getValidSimpleMoves,
  )
where

import Control.Monad (when, (<=<))
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import HChess.Board
  ( Board (..),
    Rank (..),
    Square (..),
    doesSquareContainOpponentsPiece,
    getDiagonallyAccessibleSquares,
    getPerpendicularlyAccessibleSquares,
    getPiece,
    isSquareEmpty,
    rankToPlayerRelativeRank,
    squareDown,
    squareForward,
    squareLeft,
    squareRight,
    squareUp,
  )
import HChess.Color (Color (..), oppositeColor)
import HChess.Common (findKing, startingPawnRank)
import HChess.Move (Move (..), MoveType (..))
import HChess.Piece (Piece (..), PieceType (..))
import HChess.Utils (maybeToEither, validate)

-- NOTE: This returns all moves except for castling and en passant. Also, it doesn't check if a move exposes its own king to a check.
getValidSimpleMoves :: Color -> Board -> Square -> Either String (S.Set Move)
getValidSimpleMoves currentPlayerColor board src@(Square _ srcRank) = do
  (Piece srcPieceColor srcPieceType) <- maybeToEither "No piece at specified location" $ getPiece board src
  when (srcPieceColor /= currentPlayerColor) $ Left "Can't move oponnent's piece"

  let validMoves = case srcPieceType of
        Pawn -> pawnValidMoves
        Knight -> knightValidMoves
        Bishop -> bishopValidMoves
        Rook -> rookValidMoves
        Queen -> queenValidMoves
        King -> kingValidMoves
  return validMoves
  where
    mkMove :: MoveType -> Square -> Move
    mkMove moveType dstSquare = Move src dstSquare moveType

    bishopValidMoves :: S.Set Move
    bishopValidMoves = mkMove RegularMove `S.map` getDiagonallyAccessibleSquares currentPlayerColor board src

    rookValidMoves :: S.Set Move
    rookValidMoves = mkMove RegularMove `S.map` getPerpendicularlyAccessibleSquares currentPlayerColor board src

    queenValidMoves :: S.Set Move
    queenValidMoves =
      mkMove RegularMove
        `S.map` mconcat
          [ getDiagonallyAccessibleSquares currentPlayerColor board src,
            getPerpendicularlyAccessibleSquares currentPlayerColor board src
          ]

    kingValidMoves :: S.Set Move
    kingValidMoves =
      let getKingsMoves kingsSrcSquare =
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
          filterOutMovesTooCloseToOpponentsKing kingsMoves = kingsMoves `S.difference` getKingsMoves opponentsKingSquare
       in filterOutMovesTooCloseToOpponentsKing $ getKingsMoves src

    knightValidMoves :: S.Set Move
    knightValidMoves =
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

    pawnValidMoves :: S.Set Move
    pawnValidMoves =
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
        moveOneSquareForward = mkMove RegularMove <$> (squareFwd src >>= validate (isSquareEmpty board))

        moveTwoSquaresForward :: Maybe Move
        moveTwoSquaresForward = do
          if srcRank /= startingPawnRank currentPlayerColor
            then Nothing
            else
              mkMove RegularMove
                <$> ( squareFwd src
                        >>= validate (isSquareEmpty board)
                        >>= squareFwd
                        >>= validate (isSquareEmpty board)
                    )

        attackForwardRight :: Maybe Move
        attackForwardRight = squareFwd src >>= squareRight >>= registerAsAttack

        attackForwardLeft :: Maybe Move
        attackForwardLeft = squareFwd src >>= squareLeft >>= registerAsAttack

        registerAsAttack :: Square -> Maybe Move
        registerAsAttack dstSquare
          | doesSquareContainOpponentsPiece currentPlayerColor board dstSquare = return $ mkMove RegularMove dstSquare
          | otherwise = Nothing

        detectAndLabelPawnPromotionMoves :: [Move] -> [Move]
        detectAndLabelPawnPromotionMoves = concatMap detectAndLabelPawnPromotionMove
          where
            detectAndLabelPawnPromotionMove :: Move -> [Move]
            detectAndLabelPawnPromotionMove (Move srcSquare dstSquare@(Square _ dstRank) _)
              | dstRank == rankToPlayerRelativeRank currentPlayerColor R8 =
                  Move srcSquare dstSquare . PawnPromotion <$> [Knight, Bishop, Rook, Queen]
            detectAndLabelPawnPromotionMove move = [move]
