module HChess.Check
  ( isPlayerInCheck,
  )
where

import qualified Data.Set as S
import HChess.Board
  ( Board (..),
  )
import HChess.Color (Color (..), oppositeColor)
import HChess.Common (findKing)
import HChess.Move (getMoveDstSquare)
import HChess.Piece (Piece (..))
import HChess.Utils (fromEither)
import HChess.ValidMoves.Simple (getValidSimpleMoves)

isPlayerInCheck :: Color -> Board -> Bool
isPlayerInCheck currentPlayerColor board@(Board pieces) = any isKingUnderAttackByPiece oponnentPieces
  where
    kingsSquare = findKing currentPlayerColor board
    oponnentColor = oppositeColor currentPlayerColor
    oponnentPieces = filter (\(Piece c _, _) -> c == oponnentColor) pieces
    isKingUnderAttackByPiece piece = kingsSquare `S.member` getValidDstSquaresForPiece piece
    getValidDstSquaresForPiece (Piece _ _, pieceSquare) =
      getMoveDstSquare `S.map` fromEither (getValidSimpleMoves oponnentColor board pieceSquare)
