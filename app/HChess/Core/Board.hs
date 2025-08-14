module HChess.Core.Board
  ( Board,
    Square (..),
    File (..),
    Rank (..),
    boardPieces,
    makeBoard,
    initialBoard,
    putPieceAt,
    getPieceAt,
    movePieceFromTo,
    removeAnyPieceAt,
    isSquareEmpty,
    doesSquareContainOpponentsPiece,
    getCapturedPieces,
  )
where

import Data.List (find, (\\))
import Data.Maybe (isNothing)
import HChess.Core.Board.File (File (..))
import HChess.Core.Board.Rank (Rank (..))
import HChess.Core.Board.Square (Square (..))
import HChess.Core.Color (Color (..), oppositeColor)
import HChess.Core.Piece (Piece (..), PieceType (..), color)

-- | TODO: Instead of list, use Map from Square to Piece, so that we can't have
-- invalid situations where we have multiple pieces at the same square.
newtype Board = Board [(Piece, Square)]
  deriving (Eq, Show)

boardPieces :: Board -> [(Piece, Square)]
boardPieces (Board pieces) = pieces

makeBoard :: [(Piece, Square)] -> Board
makeBoard = Board

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
    pawns color rank =
      (\f -> (Piece color Pawn, Square f rank)) <$> [FA .. FH]
    capitalPieces color rank =
      zip (Piece color <$> capitalPiecesOrder) ((`Square` rank) <$> [FA .. FH])
    capitalPiecesOrder =
      [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

-- | Puts piece at a given square while also removing any previous piece at that square.
putPieceAt :: Square -> Piece -> Board -> Board
putPieceAt sq piece = makeBoard . ((piece, sq) :) . boardPieces . removeAnyPieceAt sq

getPieceAt :: Square -> Board -> Maybe Piece
getPieceAt sq = (fst <$>) . find ((== sq) . snd) . boardPieces

-- | Moves piece from given source square to given destination square while
-- also removing any previous piece from the destination square.
movePieceFromTo :: Square -> Square -> Board -> Maybe Board
movePieceFromTo src' dst' board = do
  piece <- getPieceAt src' board
  return $ putPieceAt dst' piece $ removeAnyPieceAt src' board

removeAnyPieceAt :: Square -> Board -> Board
removeAnyPieceAt square = makeBoard . filter ((/= square) . snd) . boardPieces

isSquareEmpty :: Square -> Board -> Bool
isSquareEmpty square = isNothing . getPieceAt square

doesSquareContainOpponentsPiece :: Color -> Square -> Board -> Bool
doesSquareContainOpponentsPiece currentPlayerColor square =
  (Just (oppositeColor currentPlayerColor) ==) . ((.color) <$>) . getPieceAt square

-- | TODO: This function doesn't work fully correctly!
-- In case of pawn promotion, it will get messed up, because it will mark that pawn as captured
-- while it was not, and it will remove queen from the captured pieces if it was captured previously, ... .
-- Some other parts of code rely on this function to work correctly, so this is a real problem.
-- Solution is to not take just the Board but to take the Game (and move this function from this file somewhere else),
-- and then go through moves and actually figure out what was captured in each move.
-- So basically replaying the game.
getCapturedPieces :: Board -> [Piece]
getCapturedPieces board = initialPieces \\ currentPieces
  where
    currentPieces = fst <$> boardPieces board
    initialPieces = fst <$> boardPieces initialBoard
