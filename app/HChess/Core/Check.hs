module HChess.Core.Check
  ( isPlayerInCheck,
    findPiecesCausingCheck,
  )
where

import qualified Data.Set as S
import HChess.Core.Board (Board, Square, boardPieces)
import HChess.Core.Color (Color (..), oppositeColor)
import HChess.Core.Common (findKing)
import HChess.Core.LegalMoves.Simple (getPossibleSimpleMoves)
import HChess.Core.Move (getMoveDstSquare)
import HChess.Core.Piece (Piece (..))
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
    isKingUnderAttackBy piece = kingsSquare `S.member` getPossibleDstSquaresForPiece piece
    -- NOTE: We use `getPossibleSimpleMoves` to see if there is any opponents piece that
    --   could in next move step on and therefore eat the king, which is definition of check.
    --   `getPossibleSimpleMoves` also includes moves that expose their own king to check,
    --   which are normally not safe/legal, but they are relevant here.
    --   It does not include EnPassant and Castlings, but those can't be used to eat
    --   a king anyway so that is ok.
    getPossibleDstSquaresForPiece (Piece _ _, pieceSquare) =
      getMoveDstSquare `S.map` fromEither (getPossibleSimpleMoves oponnentColor board pieceSquare)
    pieces = boardPieces board
