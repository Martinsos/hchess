module HChess.Core.Board.Movement
  ( getDiagonallyAccessibleSquares,
    getPerpendicularlyAccessibleSquares,
  )
where

import Control.Monad ((<=<))
import qualified Data.Set as S
import HChess.Core.Board (Board, doesSquareContainOpponentsPiece, isSquareEmpty)
import HChess.Core.Board.Square (Square, squareDown, squareLeft, squareRight, squareUp)
import HChess.Core.Color (Color)

-- | Check @getAccessibleSquaresInDirection@ for detials.
getPerpendicularlyAccessibleSquares :: Color -> Board -> Square -> S.Set Square
getPerpendicularlyAccessibleSquares color =
  getAccessibleSquaresInDirections color [squareUp, squareDown, squareRight, squareLeft]

-- | Check @getAccessibleSquaresInDirection@ for detials.
getDiagonallyAccessibleSquares :: Color -> Board -> Square -> S.Set Square
getDiagonallyAccessibleSquares color =
  getAccessibleSquaresInDirections
    color
    [ squareLeft <=< squareUp,
      squareRight <=< squareUp,
      squareLeft <=< squareDown,
      squareRight <=< squareDown
    ]

-- | Check @getAccessibleSquaresInDirection@ for detials.
getAccessibleSquaresInDirections :: Color -> [Square -> Maybe Square] -> Board -> Square -> S.Set Square
getAccessibleSquaresInDirections color nextSquareInDirectionGetters board startSquare =
  mconcat $ (\f -> getAccessibleSquaresInDirection color f board startSquare) <$> nextSquareInDirectionGetters

-- | Given a square S, color of a piece P on it, and a direction D defined by a one-square move, it
-- returns all the squares in that direction that a piece P could move to in a single move, assuming
-- that P can move as much as it wants in such direction, can't jump, and can eat an oponnents piece
-- if on the way but then of course stops there.
getAccessibleSquaresInDirection ::
  Color ->
  (Square -> Maybe Square) ->
  Board ->
  Square ->
  S.Set Square
getAccessibleSquaresInDirection color getNextSquareInDirection board startSquare =
  case getNextSquareInDirection startSquare of
    Just nextSquare
      | isSquareEmpty board nextSquare ->
          S.insert nextSquare $
            getAccessibleSquaresInDirection color getNextSquareInDirection board nextSquare
      | doesSquareContainOpponentsPiece color board nextSquare -> S.singleton nextSquare
    _ -> S.empty
