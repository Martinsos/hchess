module HChess.Core.Check
  ( isPlayerInCheck,
  )
where

import qualified Data.Set as S
import HChess.Core.Board (Board, boardPieces)
import HChess.Core.Color (Color (..), oppositeColor)
import HChess.Core.Common (findKing)
import HChess.Core.Move (getMoveDstSquare)
import HChess.Core.Piece (Piece (..))
import HChess.Core.ValidMoves.Simple (getValidSimpleMoves)
import HChess.Utils (fromEither)

isPlayerInCheck :: Color -> Board -> Bool
isPlayerInCheck currentPlayerColor board = any isKingUnderAttackByPiece oponnentPieces
  where
    kingsSquare = findKing currentPlayerColor board
    oponnentColor = oppositeColor currentPlayerColor
    oponnentPieces = filter (\(Piece c _, _) -> c == oponnentColor) pieces
    isKingUnderAttackByPiece piece = kingsSquare `S.member` getValidDstSquaresForPiece piece
    getValidDstSquaresForPiece (Piece _ _, pieceSquare) =
      getMoveDstSquare `S.map` fromEither (getValidSimpleMoves oponnentColor board pieceSquare)
    pieces = boardPieces board
