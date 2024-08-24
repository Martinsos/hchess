module Main where

import Data.Char (chr, ord, toLower)
import HChess.Core.Board (Board, File (..), Rank (..), Square (..), getPieceAt)
import HChess.Core.Board.Square (squareColor)
import HChess.Core.Color (Color (..))
import HChess.Core.Game (Game, getBoard, getCurrentPlayerColor, newGame)
import HChess.Core.GameResult (GameResult (..), checkIfGameOver)
import HChess.Core.MoveOrder (MoveOrder (MoveOrder), performMoveOrder)
import HChess.Core.Piece (Piece (..), PieceType (..))

-- TODO: Write tests.
-- TODO: Separate core logic (game, move, ... -> most/all of the stuff in HChess) into a lib?

main :: IO ()
main = do
  gameLoop newGame

gameLoop :: Game -> IO ()
gameLoop game = do
  putStrLn $ unlines $ showBoardAscii $ getBoard game
  case checkIfGameOver game of
    Just result -> case result of
      Draw -> print ("Draw!" :: String)
      Victory color -> print $ show color <> " won!"
    Nothing -> do
      putStrLn $ show (getCurrentPlayerColor game) <> ", input your move:"
      game' <- readAndPerformLegalMove
      gameLoop game'
  where
    readMoveOrder :: IO MoveOrder
    readMoveOrder =
      getLine
        >>= maybe
          (putStrLn "Wrong format, must be e.g. a1-a2" >> readMoveOrder)
          pure
          . parseMoveOrder
      where
        -- parseMoveOrder <$> getLine >>= \case
        --   Nothing -> do
        --     putStrLn "Wrong format, must be e.g. a1-a2"
        --     readMoveOrder
        --   Just moveOrder -> pure moveOrder

        parseMoveOrder :: String -> Maybe MoveOrder
        parseMoveOrder str = do
          let (srcSquareStr, dstSquareStr) = (take 2 str, drop 3 str)
          srcSquare <- parseSquare srcSquareStr
          dstSquare <- parseSquare dstSquareStr
          pure $ MoveOrder srcSquare dstSquare
        parseSquare :: String -> Maybe Square
        parseSquare [f, r] = Just $ Square (toEnum $ ord (toLower f) - ord 'a') (toEnum $ ord r - ord '1')
        parseSquare _ = Nothing

    readAndPerformLegalMove :: IO Game
    readAndPerformLegalMove = do
      moveOrder <- readMoveOrder
      case performMoveOrder game moveOrder of
        Left errorMsg -> do
          print errorMsg
          readAndPerformLegalMove
        Right game' -> pure game'

showBoardAscii :: Board -> [String]
showBoardAscii board =
  [""]
    <> (showRow <$> reverse [R1 .. R8])
    <> [ "",
         "    " <> concat ((\f -> " " <> showFile f <> " ") <$> [FA .. FH])
       ]
  where
    showRow :: Rank -> String
    showRow rank =
      (showRank rank <> "   ") <> concat (showSquare . (`Square` rank) <$> [FA .. FH])

    showRank :: Rank -> String
    showRank rank = show $ 1 + fromEnum rank

    showFile :: File -> String
    showFile file = pure $ chr $ ord 'A' + fromEnum file

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
