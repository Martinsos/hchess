module Main where

import Data.List (find)
import qualified Data.Set as S
import HChess.Board (Board (..), File (..), Rank (..), Square (..))
import HChess.Game (getBoard, getValidAndSafeMoves, newGame, performMove)
import HChess.Piece (Piece (..))

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

  let game' = performMove game (head kingsPawnMoves)

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
