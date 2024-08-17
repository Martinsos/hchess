module HChess.Board
  ( Square (..),
    File (..),
    Rank (..),
    Board (..),
    initialBoard,
    getPiece,
    squareLeft,
    squareRight,
    squareUp,
    squareDown,
    squareForward,
    squareBackward,
    getDiagonallyAccessibleSquares,
    getPerpendicularlyAccessibleSquares,
    isSquareEmpty,
    doesSquareContainOpponentsPiece,
    rankToPlayerRelativeRank,
  )
where

import Control.Monad ((<=<))
import Data.Char (chr, ord)
import Data.List (find)
import Data.Maybe (isNothing)
import qualified Data.Set as S
import HChess.Color (Color (..))
import HChess.Piece (Piece (..), PieceType (..), pieceColor)
import HChess.Utils (safePred, safeSucc)

newtype Board = Board [(Piece, Square)]
  deriving (Eq, Show)

data Square = Square
  { squareFile :: !File,
    squareRank :: !Rank
  }
  deriving (Eq, Ord)

instance Show Square where
  show (Square f r) = show f ++ show r

data File = FA | FB | FC | FD | FE | FF | FG | FH
  deriving (Eq, Ord, Enum, Bounded)

instance Show File where
  show = pure . chr . (ord 'a' +) . fromEnum

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving (Eq, Ord, Enum, Bounded)

instance Show Rank where
  show = show . (+ 1) . fromEnum

getPiece :: Board -> Square -> Maybe Piece
getPiece (Board pieces) square = fst <$> find ((== square) . snd) pieces

isSquareEmpty :: Board -> Square -> Bool
isSquareEmpty board square = isNothing $ getPiece board square

doesSquareContainOpponentsPiece :: Color -> Board -> Square -> Bool
doesSquareContainOpponentsPiece currentPlayerColor board square =
  case getPiece board square of
    Just piece -> pieceColor piece /= currentPlayerColor
    Nothing -> False

squareUp :: Square -> Maybe Square
squareUp (Square f r) = (f `Square`) <$> safeSucc r

squareDown :: Square -> Maybe Square
squareDown (Square f r) = (f `Square`) <$> safePred r

squareRight :: Square -> Maybe Square
squareRight (Square f r) = (`Square` r) <$> safeSucc f

squareLeft :: Square -> Maybe Square
squareLeft (Square f r) = (`Square` r) <$> safePred f

squareForward :: Color -> Square -> Maybe Square
squareForward White = squareUp
squareForward Black = squareDown

squareBackward :: Color -> Square -> Maybe Square
squareBackward White = squareDown
squareBackward Black = squareUp

initialBoard :: Board
initialBoard =
  Board $
    concat
      [ capitalPieces Black R8,
        pawns Black R7,
        pawns White R2,
        capitalPieces White R1
      ]
  where
    pawns color rank = (\f -> (Piece color Pawn, Square f rank)) <$> [FA .. FH]
    capitalPieces color rank = zip (Piece color <$> capitalPiecesOrder) ((`Square` rank) <$> [FA .. FH])
    capitalPiecesOrder = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

rankToPlayerRelativeRank :: Color -> Rank -> Rank
rankToPlayerRelativeRank color rank = if color == White then rank else toEnum (7 - fromEnum rank)

getPerpendicularlyAccessibleSquares :: Color -> Board -> Square -> S.Set Square
getPerpendicularlyAccessibleSquares color =
  getAccessibleSquaresInDirections color [squareUp, squareDown, squareRight, squareLeft]

getDiagonallyAccessibleSquares :: Color -> Board -> Square -> S.Set Square
getDiagonallyAccessibleSquares color =
  getAccessibleSquaresInDirections
    color
    [ squareLeft <=< squareUp,
      squareRight <=< squareUp,
      squareLeft <=< squareDown,
      squareRight <=< squareDown
    ]

getAccessibleSquaresInDirections :: Color -> [Square -> Maybe Square] -> Board -> Square -> S.Set Square
getAccessibleSquaresInDirections color nextSquareInDirectionGetters board startSquare =
  mconcat $ (\f -> getAccessibleSquaresInDirection color f board startSquare) <$> nextSquareInDirectionGetters

-- TODO: Define (in comment) what accessible means (and what this function does).
getAccessibleSquaresInDirection :: Color -> (Square -> Maybe Square) -> Board -> Square -> S.Set Square
getAccessibleSquaresInDirection color getNextSquareInDirection board startSquare =
  case getNextSquareInDirection startSquare of
    Just nextSquare
      | isSquareEmpty board nextSquare ->
          S.insert nextSquare $
            getAccessibleSquaresInDirection color getNextSquareInDirection board nextSquare
      | doesSquareContainOpponentsPiece color board nextSquare -> S.singleton nextSquare
    _ -> S.empty

onSameDiagonal :: Square -> Square -> Bool
onSameDiagonal (Square f1 r1) (Square f2 r2) = fromEnum f1 - fromEnum r1 == fromEnum f2 - fromEnum r2

onSameRank :: Square -> Square -> Bool
onSameRank (Square _ r1) (Square _ r2) = r1 == r2

onSameFile :: Square -> Square -> Bool
onSameFile (Square f1 _) (Square f2 _) = f1 == f2
