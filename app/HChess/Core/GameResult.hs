module HChess.Core.GameResult
  ( GameResult (..),
    checkIfGameOver,
  )
where

import HChess.Core.Board (Square, boardPieces)
import HChess.Core.Check (isPlayerInCheck)
import HChess.Core.Color (Color, oppositeColor)
import HChess.Core.Game (Game, getBoard, getCurrentPlayerColor)
import HChess.Core.LegalMoves (getLegalMoves)
import HChess.Core.Piece (Piece (..))
import HChess.Utils (fromEither)

data GameResult = Victory !Color | Draw

-- | Returns game result if game is over, or Nothing if game is not over yet.
checkIfGameOver :: Game -> Maybe GameResult
checkIfGameOver game
  | playerHasNoLegalMoves currentPlayerColor =
      if isPlayerInCheck currentPlayerColor board
        then Just $ Victory $ oppositeColor currentPlayerColor
        else Just Draw
  | noHappeningsIn50Moves = Just Draw
  | insufficientMaterial = Just Draw
  | otherwise = Nothing
  where
    (board, currentPlayerColor) = (getBoard game, getCurrentPlayerColor game)

    playerHasNoLegalMoves :: Color -> Bool
    playerHasNoLegalMoves playerColor =
      null (mconcat $ map (fromEither . getLegalMoves game) (squaresWithPiecesOfColor playerColor))

    squaresWithPiecesOfColor :: Color -> [Square]
    squaresWithPiecesOfColor color =
      snd <$> filter (\(Piece c _, _) -> c == color) (boardPieces board)

    -- No piece has been captured and no pawn has been moved with a period of 50 moves.
    noHappeningsIn50Moves :: Bool
    noHappeningsIn50Moves = False -- TODO: Implement!

    --  Neither players has enough pieces to deliver checkmate. Game is a draw.
    --  Possible cases:
    --    King vs king with no other pieces.
    --    King and bishop vs king.
    --    King and knight vs king.
    --    King and bishop vs king and bishop of the same coloured square.
    --  This is not so simple, there are other ways to define if it is insufficient material,
    --  and some are better while some are worse, and all of them are a bit imprecise from what I got.
    insufficientMaterial :: Bool
    insufficientMaterial = False -- TODO: Implement!
