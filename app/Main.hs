module Main where

import Control.Monad (when, (<=<))
import Data.Char (chr, ord)
import Data.Foldable (find)
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import qualified Data.Set as S

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Game = Game [Move]
  deriving (Eq)

data Color = White | Black
  deriving (Eq, Show)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Enum, Ord)

data Piece = Piece Color PieceType
  deriving (Eq, Show)

data Move = Move Square Square
  deriving (Eq)

data Square = Square File Rank
  deriving (Eq, Ord)

data File = FA | FB | FC | FD | FE | FF | FG | FH
  deriving (Eq, Ord, Enum, Bounded)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving (Eq, Ord, Enum, Bounded)

data Board = Board [(Piece, Square)]
  deriving (Eq, Show)

instance Show Move where
  show (Move from to) = show from ++ "-" ++ show to

instance Show Square where
  show (Square f r) = show f ++ show r

instance Show File where
  show = show . chr . (ord 'a' +) . fromEnum

instance Show Rank where
  show = show . (+ 1) . fromEnum

instance Show PieceType where
  show piece = case piece of
    Pawn -> "i"
    Knight -> "N"
    Bishop -> "B"
    Rook -> "R"
    Queen -> "Q"
    King -> "K"

type InvalidMoveMsg = String

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
    capitalPieces color rank = zip (Piece color <$> capitalPiecesOrder) ((\f -> Square f rank) <$> [FA .. FH])
    capitalPiecesOrder = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

getValidMoves :: Game -> Square -> Either String (S.Set Square)
getValidMoves game src@(Square srcFile srcRank) = do
  (Piece srcPieceColor srcPieceType) <- maybeToEither "No piece at specified location" $ getPiece board src
  when (srcPieceColor /= currentPlayerColor) $ Left "Can't move oponnent's piece"
  let validMoves = case srcPieceType of
        Pawn ->
          let startingPawnRank = if currentPlayerColor == White then R2 else R7
              squareFwd = squareForward currentPlayerColor
              maybeSquareForward = squareFwd src >>= validate (isSquareEmpty board)
              maybeTwoSquaresForward = do
                if srcRank /= startingPawnRank
                  then Nothing
                  else
                    squareFwd src
                      >>= validate (isSquareEmpty board)
                      >>= squareFwd
                      >>= validate (isSquareEmpty board)
              maybeForwardRightSquare =
                -- TODO: Take en-passant into consideration.
                squareFwd src
                  >>= squareRight
                  >>= validate (doesSquareContainOpponentsPiece currentPlayerColor board)
              maybeForwardLeftSquare =
                -- TODO: Take en-passant into consideration.
                squareFwd src
                  >>= squareLeft
                  >>= validate (doesSquareContainOpponentsPiece currentPlayerColor board)
           in (S.fromList . catMaybes)
                [ maybeSquareForward,
                  maybeTwoSquaresForward,
                  maybeForwardRightSquare,
                  maybeForwardLeftSquare
                ]
        Knight ->
          (S.fromList . mapMaybe ($ src))
            [ squareUp <=< squareUp <=< squareRight,
              squareUp <=< squareUp <=< squareLeft,
              squareUp <=< squareRight <=< squareRight,
              squareUp <=< squareLeft <=< squareLeft,
              squareDown <=< squareDown <=< squareRight,
              squareDown <=< squareDown <=< squareLeft,
              squareDown <=< squareRight <=< squareRight,
              squareDown <=< squareLeft <=< squareLeft
            ]
        Bishop -> getDiagonallyAccessibleSquares board src
        Rook -> getPerpendicularlyAccessibleSquares board src
        Queen -> getDiagonallyAccessibleSquares board src <> getPerpendicularlyAccessibleSquares board src
        King ->
          -- TODO: Filter out moves that are too close to oponnent's king!
          -- TODO: Allow castling!
          (S.fromList . mapMaybe ($ src))
            [ squareUp,
              squareUp <=< squareLeft,
              squareUp <=< squareRight,
              squareLeft,
              squareRight,
              squareDown,
              squareDown <=< squareLeft,
              squareDown <=< squareRight
            ]
  -- TODO: Filter out moves that cause player's king to be under attack!
  return validMoves
  where
    board = getBoard game
    currentPlayerColor = getCurrentPlayerColor game

getBoard :: Game -> Board
getBoard game = error "TODO: Either calculate it by using moves, or get it from Game if it is stored in there."

getCurrentPlayerColor :: Game -> Color
getCurrentPlayerColor (Game moves) = if even (length moves) then White else Black

getPerpendicularlyAccessibleSquares :: Board -> Square -> S.Set Square
getPerpendicularlyAccessibleSquares =
  getAccessibleSquaresInDirections [squareUp, squareDown, squareRight, squareLeft]

getDiagonallyAccessibleSquares :: Board -> Square -> S.Set Square
getDiagonallyAccessibleSquares =
  getAccessibleSquaresInDirections
    [ squareLeft <=< squareUp,
      squareRight <=< squareUp,
      squareLeft <=< squareDown,
      squareRight <=< squareDown
    ]

getAccessibleSquaresInDirections :: [Square -> Maybe Square] -> Board -> Square -> S.Set Square
getAccessibleSquaresInDirections nextSquareInDirectionGetters board startSquare =
  mconcat $ (\f -> getAccessibleSquaresInDirection f board startSquare) <$> nextSquareInDirectionGetters

getAccessibleSquaresInDirection :: (Square -> Maybe Square) -> Board -> Square -> S.Set Square
getAccessibleSquaresInDirection getNextSquareInDirection board startSquare =
  S.delete startSquare $ S.fromList $ go startSquare
  where
    go :: Square -> [Square]
    go sq =
      let emptyNextSquare = validate (isSquareEmpty board) =<< getNextSquareInDirection sq
       in sq : maybe [] go emptyNextSquare

validate :: (a -> Bool) -> a -> Maybe a
validate p a = if p a then Just a else Nothing

onSameDiagonal :: Square -> Square -> Bool
onSameDiagonal (Square f1 r1) (Square f2 r2) = fromEnum f1 - fromEnum r1 == fromEnum f2 - fromEnum r2

onSameRank :: Square -> Square -> Bool
onSameRank (Square _ r1) (Square _ r2) = r1 == r2

onSameFile :: Square -> Square -> Bool
onSameFile (Square f1 _) (Square f2 _) = f1 == f2

isPathStraightAndFree :: Board -> Square -> Square -> Bool
isPathStraightAndFree = error "TODO: check if path betwen two squares is straight and contains no pieces (start and end squares can contain pieces)."

getPiece :: Board -> Square -> Maybe Piece
getPiece (Board pieces) square = fst <$> find ((== square) . snd) pieces

isSquareEmpty :: Board -> Square -> Bool
isSquareEmpty board square = isNothing $ getPiece board square

doesSquareContainOpponentsPiece :: Color -> Board -> Square -> Bool
doesSquareContainOpponentsPiece color board square = case getPiece board square of
  Just piece -> pieceColor piece == color
  Nothing -> False

pieceColor :: Piece -> Color
pieceColor (Piece color _) = color

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a

safeSucc :: (Bounded a, Eq a) => a -> Maybe a
safeSucc a = if a == maxBound then Nothing else Just a

safePred :: (Bounded a, Eq a) => a -> Maybe a
safePred a = if a == minBound then Nothing else Just a

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
