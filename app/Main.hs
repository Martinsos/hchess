module Main where

import Data.Char (chr, ord, toLower)
import HChess.Core.Board (Board, File (..), Rank (..), Square (..), getPieceAt)
import HChess.Core.Board.Square (squareColor)
import HChess.Core.Check (isPlayerInCheck)
import HChess.Core.Color (Color (..))
import HChess.Core.Game (Game, getBoard, getCurrentPlayerColor, newGame)
import HChess.Core.GameResult (GameResult (..), checkIfGameOver)
import HChess.Core.MoveOrder (MoveOrder (MoveOrder), performMoveOrder)
import HChess.Core.Piece (Piece (..), PieceType (..))
import HChess.Utils (safeToEnum)

-- TODO: Write tests.
-- TODO: Separate core logic (game, move, ... -> most/all of the stuff in HChess) into a lib?
-- TODO: Separate ASCII playing into its own module tree (HChess.Ascii) and also
-- implement alternative frontends like Brick (HChess.Terminal) and also real GUI (HChess.GUI).
-- Maybe group them all under `HChess.Frontend`.

main :: IO ()
main = do
  gameLoop newGame

gameLoop :: Game -> IO ()
gameLoop game = do
  clearScreen

  putStrLn $ unlines $ showBoardAscii $ getBoard game

  -- TODO: Print pieces that are out of the game, in two rows, black and white.
  -- TODO: Print all the moves that happened so far, in chess notation.

  putStrLn $
    if isPlayerInCheck (getCurrentPlayerColor game) (getBoard game)
      then "Check!"
      else ""

  case checkIfGameOver game of
    Just result -> case result of
      Draw -> putStrLn "Draw!"
      Victory color -> putStrLn $ show color <> " won!"
    Nothing -> do
      putStrLn $ show (getCurrentPlayerColor game) <> ", input your move:"
      game' <- readAndPerformLegalMove game
      gameLoop game'

readAndPerformLegalMove :: Game -> IO Game
readAndPerformLegalMove game = do
  moveOrder <- readMoveOrder
  case performMoveOrder game moveOrder of
    Left errorMsg -> print errorMsg >> readAndPerformLegalMove game
    Right game' -> pure game'
  where
    readMoveOrder :: IO MoveOrder
    readMoveOrder =
      getLine
        >>= maybe
          (putStrLn "Wrong format, must be e.g. a1-a2" >> readMoveOrder)
          pure
          . parseMoveOrder
      where
        parseMoveOrder :: String -> Maybe MoveOrder
        parseMoveOrder str = do
          let (srcSquareStr, dstSquareStr) = (take 2 str, drop 3 str)
          srcSquare <- parseSquare srcSquareStr
          dstSquare <- parseSquare dstSquareStr
          pure $ MoveOrder srcSquare dstSquare

        parseSquare :: String -> Maybe Square
        parseSquare [fileChar, rankChar] = do
          file <- safeToEnum $ ord (toLower fileChar) - ord 'a'
          rank <- safeToEnum $ ord rankChar - ord '1'
          return $ Square file rank
        parseSquare _ = Nothing

clearScreen :: IO ()
clearScreen = do
  putStrLn "\ESC[2J"
  putStrLn "\ESC[H"

showBoardAscii :: Board -> [String]
showBoardAscii board =
  (showRow <$> reverse [R1 .. R8])
    <> ["\n    " <> concat ((\f -> " " <> showFileNumber f <> " ") <$> [FA .. FH])]
  where
    showRow :: Rank -> String
    showRow rank =
      (showRankLetter rank <> "   ") <> concat (showSquare . (`Square` rank) <$> [FA .. FH])

    showRankLetter :: Rank -> String
    showRankLetter rank = show $ 1 + fromEnum rank

    showFileNumber :: File -> String
    showFileNumber file = pure $ chr $ ord 'A' + fromEnum file

    showSquare :: Square -> String
    showSquare square =
      concat
        [ case squareColor square of
            White -> "\ESC[45m"
            Black -> "\ESC[40m",
          " ",
          maybe " " showPiece (getPieceAt square board),
          " ",
          "\ESC[0m"
        ]

    showPiece :: Piece -> String
    showPiece (Piece Black pType) = case pType of
      King -> "\x2654"
      Queen -> "\x2655"
      Rook -> "\x2656"
      Bishop -> "\x2657"
      Knight -> "\x2658"
      Pawn -> "\x2659"
    showPiece (Piece White pType) = case pType of
      King -> "\x265A"
      Queen -> "\x265B"
      Rook -> "\x265C"
      Bishop -> "\x265D"
      Knight -> "\x265E"
      Pawn -> "\x265F"
