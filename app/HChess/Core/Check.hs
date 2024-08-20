module HChess.Core.Check
  ( isPlayerInCheck,
    findPiecesCausingCheck,
  )
where

import qualified Data.Set as S
import HChess.Core.Board (Board, Square, boardPieces)
import HChess.Core.Color (Color (..), oppositeColor)
import HChess.Core.Common (findKing)
import HChess.Core.Move (getMoveDstSquare)
import HChess.Core.Piece (Piece (..))
import HChess.Core.ValidMoves.Simple (getValidSimpleMoves)
import HChess.Utils (fromEither)

isPlayerInCheck :: Color -> Board -> Bool
isPlayerInCheck currentPlayerColor board =
  not $ null $ findPiecesCausingCheck currentPlayerColor board

findPiecesCausingCheck :: Color -> Board -> [(Piece, Square)]
findPiecesCausingCheck currentPlayerColor board = isKingUnderAttackBy `filter` oponnentPieces
  where
    kingsSquare = findKing currentPlayerColor board
    oponnentColor = oppositeColor currentPlayerColor
    oponnentPieces = filter (\(Piece c _, _) -> c == oponnentColor) pieces
    isKingUnderAttackBy piece = kingsSquare `S.member` getValidDstSquaresForPiece piece
    -- NOTE: We use `getValidSimpleMoves` to see if there is any opponents piece that
    --   could in next move step on and therefore eat the king, which is definition of check.
    --   `getValidSimpleMoves` also includes moves that expose their own king to check,
    --   which are normally not safe/legal, but they are relevant here.
    --   It does not include EnPassant and Castlings, but those can't be used to eat
    --   a king anyway so that is ok.
    getValidDstSquaresForPiece (Piece _ _, pieceSquare) =
      getMoveDstSquare `S.map` fromEither (getValidSimpleMoves oponnentColor board pieceSquare)
    pieces = boardPieces board
