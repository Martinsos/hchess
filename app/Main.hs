module Main where

import Data.Either (fromRight)
import Data.List (find)
import qualified Data.Set as S
import HChess.Board (Board (..), File (..), Rank (..), Square (..))
import HChess.Game (getBoard, newGame)
import HChess.MoveOrder (MoveOrder (MoveOrder), performMoveOrder)
import HChess.Piece (Piece (..))
import HChess.ValidMoves (getValidAndSafeMoves)

-- TODO: Write tests.
-- TODO: Separate core logic (game, move, ... -> most/all of the stuff in HChess) into a lib.

main :: IO ()
main = do
  let game = newGame

  putStrLn "\nBoard:"
  putStrLn $ unlines $ showBoard $ getBoard game

  putStrLn "\nValid moves for the most-left white horsey:"
  print $ getValidAndSafeMoves game (Square FB R1)

  let Right kingsPawnMoves = S.toList <$> getValidAndSafeMoves game (Square FE R2)
  putStrLn "\nValid moves for the king's pawn:"
  print kingsPawnMoves

  let game' =
        fromRight (error "invalid move") $
          performMoveOrder game $ MoveOrder (Square FE R2) (Square FE R4)

  putStrLn "\nBoard:"
  putStrLn $ unlines $ showBoard $ getBoard game'

showBoard :: Board -> [String]
showBoard (Board piecesOnBoard) = showRow <$> reverse [R1 .. R8]
  where
    showRow :: Rank -> String
    showRow rank = concat $ showSquare . (`Square` rank) <$> [FA .. FH]

    showSquare :: Square -> String
    showSquare square =
      case find ((== square) . snd) piecesOnBoard of
        Nothing -> "-"
        Just (Piece _color pieceType, _square) -> show pieceType
