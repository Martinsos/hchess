module Main where

import Data.Either (fromRight)
import Data.List (find)
import qualified Data.Set as S
import HChess.Core.Board (Board, File (..), Rank (..), Square (..), boardPieces)
import HChess.Core.Game (getBoard, newGame)
import HChess.Core.LegalMoves (getLegalMoves)
import HChess.Core.MoveOrder (MoveOrder (MoveOrder), performMoveOrder)
import HChess.Core.Piece (Piece (..))

-- TODO: Write tests.
-- TODO: Separate core logic (game, move, ... -> most/all of the stuff in HChess) into a lib.

main :: IO ()
main = do
  let game = newGame

  putStrLn "\nBoard:"
  putStrLn $ unlines $ showBoard $ getBoard game

  putStrLn "\nLegal moves for the most-left white horsey:"
  print $ getLegalMoves game (Square FB R1)

  let Right kingsPawnMoves = S.toList <$> getLegalMoves game (Square FE R2)
  putStrLn "\nLegal moves for the king's pawn:"
  print kingsPawnMoves

  let game' =
        fromRight (error "illegal move") $
          performMoveOrder game $ MoveOrder (Square FE R2) (Square FE R4)

  putStrLn "\nBoard:"
  putStrLn $ unlines $ showBoard $ getBoard game'

showBoard :: Board -> [String]
showBoard board = showRow <$> reverse [R1 .. R8]
  where
    showRow :: Rank -> String
    showRow rank = concat $ showSquare . (`Square` rank) <$> [FA .. FH]

    showSquare :: Square -> String
    showSquare square =
      case find ((== square) . snd) piecesOnBoard of
        Nothing -> "-"
        Just (piece, _square) -> show $ pieceType piece

    piecesOnBoard = boardPieces board
