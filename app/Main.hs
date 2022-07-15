module Main where

import Control.Monad (unless, when)
import Data.Char (chr, ord)
import Data.Foldable (find)
import Data.Maybe (fromJust, isNothing)

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
  deriving (Eq)

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

-- performMove :: Board -> Move -> Either InvalidMoveMsg Board
-- performMove board (Move src dst) =
--     -- TODO: Verify that own king is not checkmated by the move!
--     if checkIfValidMove board src dst
--        then error "TODO"
--        else undefined

checkIfValidMove :: Board -> Color -> Move -> Either InvalidMoveMsg ()
checkIfValidMove board playerColor (Move src@(Square srcFile srcRank) dst@(Square dstFile dstRank)) = do
  (Piece srcPieceColor srcPieceType) <- maybeToEither "No piece at specified location" $ getPiece board src
  when (srcPieceColor /= playerColor) $ Left "Can't move oponnent's piece"
  case srcPieceType of
    Pawn ->
      let squareForward' = squareForward playerColor
          startingRank = if playerColor == White then R2 else R7
          isMovingOneRankForward = Just dst == squareForward' src
          isMovingTwoRanksForwardFromStartingRank =
            srcRank == startingRank
              && Just dst == (squareForward' =<< squareForward' src)
              && isPathStraightAndFree board src dst
       in unless (onSameFile src dst && (isMovingOneRankForward || isMovingTwoRanksForwardFromStartingRank)) $
            Left "Invalid move"
    Knight ->
      unless
        ( Just dst
            `elem` [ squareUp =<< squareUp =<< squareRight src,
                     squareUp =<< squareUp =<< squareLeft src,
                     squareUp =<< squareRight =<< squareRight src,
                     squareUp =<< squareLeft =<< squareLeft src,
                     squareDown =<< squareDown =<< squareRight src,
                     squareDown =<< squareDown =<< squareLeft src,
                     squareDown =<< squareRight =<< squareRight src,
                     squareDown =<< squareLeft =<< squareLeft src
                   ]
        )
        $ Left "Invalid move"
    Bishop -> unless (onSameDiagonal src dst && isPathStraightAndFree board src dst) $ Left "Invalid move"
    Rook -> unless ((onSameFile src dst || onSameRank src dst) && isPathStraightAndFree board src dst) $ Left "Invalid move"
    Queen -> unless (isPathStraightAndFree board src dst) $ Left "Invalid move"
    King ->
      unless
        ( Just dst
            `elem` [ squareUp src,
                     squareUp =<< squareLeft src,
                     squareUp =<< squareRight src,
                     squareLeft src,
                     squareRight src,
                     squareDown src,
                     squareDown =<< squareLeft src,
                     squareDown =<< squareRight src
                   ]
        )
        $ Left "Invalid move"
  when ((pieceColor <$> getPiece board dst) == Just playerColor) $
    Left "Blocked by your own piece."

-- TODO: Check if king is trying to castle -> if so, allow it. But check comment below, I will need history to know if castle or king moved!
-- TODO: Check if king gets exposed during the move -> if it does, move is invalid.
-- TODO: Allow en-passant. Although for that I need to know previous move, right? Hm that means I need to know history, which I don't have at the moment.
--  Actually, I will also need history to figure out castling!
--  Ok so I need to probably change signature to isValidMove :: Game -> Square -> Square -> Either InvalidMoveMsg ()

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
