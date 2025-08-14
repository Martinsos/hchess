module Main where

import Control.Monad (forM_)
import Data.Char (chr, ord, toLower)
import Data.List (sort)
import HChess.Core.AlgebraicNotation (getStdNotationForGame)
import HChess.Core.Board (Board, File (..), Rank (..), Square (..), getCapturedPieces, getPieceAt)
import HChess.Core.Board.Square (squareColor)
import HChess.Core.Check (isPlayerInCheck)
import HChess.Core.Color (Color (..))
import HChess.Core.Game (Game, getBoard, getCurrentPlayerColor, newGame)
import HChess.Core.GameResult (GameResult (..), checkIfGameOver)
import HChess.Core.MoveOrder (MoveOrder (MoveOrder), performMoveOrder)
import HChess.Core.Piece (Piece (..), pieceToUnicode)
import HChess.Utils (safeToEnum)

-- TODO: Separate core logic (game, move, ... -> most/all of the stuff in HChess) into a lib?
-- TODO: Should I organize code a bit differently? Extract more advanced logic from core files
--   like Move, Board, and similar and group it under Rules (Start, End, Moves, ...)?
-- TODO: Separate ASCII playing into its own module tree (HChess.Ascii) and also
-- implement alternative frontends like Brick (HChess.Terminal) and also real GUI (HChess.GUI).
-- Maybe group them all under `HChess.Frontend`.
-- TODO: Update README.md .

main :: IO ()
main = do
  gameLoop newGame

gameLoop :: Game -> IO ()
gameLoop game = do
  clearScreen
  printBoard
  printCapturedPieces
  printStdNotation
  printCheckStatus
  case checkIfGameOver game of
    Just result -> printGameResult result
    Nothing -> readAndPerformLegalMove game >>= gameLoop
  where
    board = getBoard game

    currentColor = getCurrentPlayerColor game

    printBoard = putStrLn $ unlines $ showBoardAscii board

    printCheckStatus =
      putStrLn $ if isPlayerInCheck currentColor board then "Check!" else ""

    printGameResult result = case result of
      Draw -> putStrLn "Draw!"
      Victory color -> putStrLn $ show color <> " won!"

    printCapturedPieces = do
      let capturedPieces = getCapturedPieces board
      forM_
        [White, Black]
        ( \color ->
            (putStrLn . concatMap pieceToUnicode . sort . filter ((== color) . (.color)))
              capturedPieces
        )

    printStdNotation = do
      let moves = getStdNotationForGame game
      -- TODO: Group movs by turns (white and black) prefixed with integers.
      --   Check wikipedia for details.
      print $ reverse moves

readAndPerformLegalMove :: Game -> IO Game
readAndPerformLegalMove game = do
  putStrLn $ show (getCurrentPlayerColor game) <> ", input your move:"
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
    showFileNumber file = pure $ chr $ ord 'a' + fromEnum file

    showSquare :: Square -> String
    showSquare square =
      concat
        [ case squareColor square of
            White -> "\ESC[45m"
            Black -> "\ESC[40m",
          " ",
          maybe " " pieceToUnicode (getPieceAt square board),
          " ",
          "\ESC[0m"
        ]
