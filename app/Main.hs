module Main where

import Control.Monad (when, (<=<))
import Data.Char (chr, ord)
import Data.Foldable (find, foldl')
import Data.Maybe (catMaybes, fromJust, isNothing, mapMaybe)
import qualified Data.Set as S

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Game = Game [Move]
  deriving (Eq)

data Color = White | Black
  deriving (Eq, Show)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Enum, Ord)

data Piece = Piece Color PieceType
  deriving (Eq, Show)

data Move = Move Square Square MoveType
  deriving (Eq, Ord)

data MoveType = RegularMove | EnPassant | KingsideCastling | QueensideCastling | PawnPromotion PieceType
  deriving (Eq, Show, Ord)

data Square = Square File Rank
  deriving (Eq, Ord)

data File = FA | FB | FC | FD | FE | FF | FG | FH
  deriving (Eq, Ord, Enum, Bounded)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving (Eq, Ord, Enum, Bounded)

data Board = Board [(Piece, Square)]
  deriving (Eq, Show)

data GameResult = Victory Color | Draw

instance Show Move where
  show (Move from to _) = show from ++ "-" ++ show to

instance Show Square where
  show (Square f r) = show f ++ show r

instance Show File where
  show = show . chr . (ord 'a' +) . fromEnum

instance Show Rank where
  show = show . (+ 1) . fromEnum

instance Show PieceType where
  show piece = case piece of
    Pawn -> "i"
    Knight -> "N"
    Bishop -> "B"
    Rook -> "R"
    Queen -> "Q"
    King -> "K"

initialBoard :: Board
initialBoard =
  Board $
    concat
      [ capitalPieces Black R8,
        pawns Black R7,
        pawns White R2,
        capitalPieces White R1
      ]
  where
    pawns color rank = (\f -> (Piece color Pawn, Square f rank)) <$> [FA .. FH]
    capitalPieces color rank = zip (Piece color <$> capitalPiecesOrder) ((\f -> Square f rank) <$> [FA .. FH])
    capitalPiecesOrder = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

-- "TODO: I should keep the current board in the Game itself and this would be just getter.
getBoard :: Game -> Board
getBoard (Game moves) = error "TODO"

isGameOver :: Game -> GameResult
isGameOver = error "TODO"

isCurrentPlayerInCheck :: Game -> Bool
isCurrentPlayerInCheck = error "TODO"

-- TODO: Perform move on the board, then add move to log of moves and set new board.
--  Don't do move if game is done.
performMove :: Game -> Move -> Either String Game
performMove (Game moves) move = error "TODO"

performMoveOnBoard :: Board -> Move -> Either String Board
performMoveOnBoard board move =
  -- TODO:
  -- 1. Check if move is valid by checking against all valid moves of the piece we are trying to move.
  -- 2. Move the piece from src square to dst square.
  -- 3. Remove opponents piece if it is present on the dst square.
  -- 4. Handle special cases (en passant, castlings, pawn promotion).
  error "TODO"

-- TODO: Very often I am passing around relatively boring stuff like board, currentPlayerColor, and game.
--   Maybe I should make a Reader monad that just contains Game in its Reader, since from Game we can then access all of this info?
--   And it would reduce clutter a bit? For functions that are happening in the context of current state of the game.

-- TODO: This function is huge, take it out into separate module and move complex functions in @where@ to standalone functions.
getValidMoves :: Game -> Square -> Either String (S.Set Move)
getValidMoves game src@(Square _ srcRank) = do
  (Piece srcPieceColor srcPieceType) <- maybeToEither "No piece at specified location" $ getPiece board src
  when (srcPieceColor /= currentPlayerColor) $ Left "Can't move oponnent's piece"
  let validMoves = case srcPieceType of
        Pawn -> pawnValidMoves
        Knight -> knightValidMoves
        Bishop -> bishopValidMoves
        Rook -> rookValidMoves
        Queen -> queenValidMoves
        King -> kingValidMoves
  return $ S.filter (not . doesMovePutOwnKingInCheck) validMoves
  where
    board :: Board
    board = getBoard game

    currentPlayerColor :: Color
    currentPlayerColor = getCurrentPlayerColor game

    mkMove :: MoveType -> Square -> Move
    mkMove moveType dstSquare = Move src dstSquare moveType

    kingsSquare :: Square
    kingsSquare = findKing currentPlayerColor board

    doesMovePutOwnKingInCheck :: Move -> Bool
    doesMovePutOwnKingInCheck move = isSquareUnderAttackByCurrentPlayer gameAfterMove kingsSquare
      where
        gameAfterMove = fromEither $ performMove game move

    bishopValidMoves :: S.Set Move
    bishopValidMoves = mkMove RegularMove `S.map` getDiagonallyAccessibleSquares currentPlayerColor board src

    rookValidMoves :: S.Set Move
    rookValidMoves = mkMove RegularMove `S.map` getPerpendicularlyAccessibleSquares currentPlayerColor board src

    queenValidMoves :: S.Set Move
    queenValidMoves =
      mkMove RegularMove
        `S.map` mconcat
          [ getDiagonallyAccessibleSquares currentPlayerColor board src,
            getPerpendicularlyAccessibleSquares currentPlayerColor board src
          ]

    kingValidMoves :: S.Set Move
    kingValidMoves =
      -- TODO: Allow castling!
      let getKingsMoves kingsSrcSquare =
            (S.fromList . (mkMove RegularMove <$>) . mapMaybe ($ kingsSrcSquare))
              [ squareUp,
                squareUp <=< squareLeft,
                squareUp <=< squareRight,
                squareLeft,
                squareRight,
                squareDown,
                squareDown <=< squareLeft,
                squareDown <=< squareRight
              ]
          opponentsKingSquare = findKing (oppositeColor currentPlayerColor) board
          filterOutMovesTooCloseToOpponentsKing kingsMoves = kingsMoves `S.difference` getKingsMoves opponentsKingSquare
       in filterOutMovesTooCloseToOpponentsKing $ getKingsMoves src

    knightValidMoves :: S.Set Move
    knightValidMoves =
      (S.fromList . (mkMove RegularMove <$>) . mapMaybe ($ src))
        [ squareUp <=< squareUp <=< squareRight,
          squareUp <=< squareUp <=< squareLeft,
          squareUp <=< squareRight <=< squareRight,
          squareUp <=< squareLeft <=< squareLeft,
          squareDown <=< squareDown <=< squareRight,
          squareDown <=< squareDown <=< squareLeft,
          squareDown <=< squareRight <=< squareRight,
          squareDown <=< squareLeft <=< squareLeft
        ]

    pawnValidMoves :: S.Set Move
    pawnValidMoves =
      (S.fromList . detectAndLabelPawnPromotionMoves . catMaybes)
        [ moveOneSquareForward,
          moveTwoSquaresForward,
          attackForwardRight,
          attackForwardLeft
        ]
      where
        squareFwd :: Square -> Maybe Square
        squareFwd = squareForward currentPlayerColor

        moveOneSquareForward :: Maybe Move
        moveOneSquareForward = mkMove RegularMove <$> (squareFwd src >>= validate (isSquareEmpty board))

        moveTwoSquaresForward :: Maybe Move
        moveTwoSquaresForward = do
          if srcRank /= startingPawnRank currentPlayerColor
            then Nothing
            else
              mkMove RegularMove
                <$> ( squareFwd src
                        >>= validate (isSquareEmpty board)
                        >>= squareFwd
                        >>= validate (isSquareEmpty board)
                    )

        attackForwardRight :: Maybe Move
        attackForwardRight = squareFwd src >>= squareRight >>= registerAsAttack

        attackForwardLeft :: Maybe Move
        attackForwardLeft = squareFwd src >>= squareLeft >>= registerAsAttack

        registerAsAttack :: Square -> Maybe Move
        registerAsAttack dstSquare
          | doesSquareContainOpponentsPiece currentPlayerColor board dstSquare = return $ mkMove RegularMove dstSquare
          | isMoveEnPassant game dstSquare = return $ mkMove EnPassant dstSquare
          | otherwise = Nothing

        detectAndLabelPawnPromotionMoves :: [Move] -> [Move]
        detectAndLabelPawnPromotionMoves = concatMap detectAndLabelPawnPromotionMove
          where
            detectAndLabelPawnPromotionMove :: Move -> [Move]
            detectAndLabelPawnPromotionMove (Move srcSquare dstSquare@(Square _ dstRank) _)
              | dstRank == rankToPlayerRelativeRank currentPlayerColor R8 =
                  Move srcSquare dstSquare . PawnPromotion <$> [Knight, Bishop, Rook, Queen]
            detectAndLabelPawnPromotionMove move = [move]

findKing :: Color -> Board -> Square
findKing color (Board pieces) = snd $ fromJust $ find (\(Piece c t, _) -> c == color && t == King) pieces

getCurrentPlayerColor :: Game -> Color
getCurrentPlayerColor (Game moves) = if even (length moves) then White else Black

oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White

getMoveDstSquare :: Move -> Square
getMoveDstSquare (Move _ dstSquare _) = dstSquare

isSquareUnderAttackByCurrentPlayer :: Game -> Square -> Bool
isSquareUnderAttackByCurrentPlayer game square =
  any (isSquareUnderAttackByPiece square) currentPlayerPieces
  where
    (Board pieces) = getBoard game
    currentPlayerPieces = filter (\(Piece c _, _) -> c == currentPlayerColor) pieces
    currentPlayerColor = getCurrentPlayerColor game
    getValidDstSquaresForPiece (Piece _ _, pieceSquare) = getMoveDstSquare `S.map` fromEither (getValidMoves game pieceSquare)
    isSquareUnderAttackByPiece sq piece = sq `S.member` getValidDstSquaresForPiece piece

fromEither :: Either a b -> b
fromEither (Right x) = x
fromEither (Left _) = error "Encountered Left, but expected Right"

-- | For a given destination square, determine if landing a pawn on that square would be en passant move.
isMoveEnPassant :: Game -> Square -> Bool
isMoveEnPassant (Game []) _ = False
isMoveEnPassant game@(Game ((Move lastMoveSrcSquare@(Square _ lastMoveSrcSquareRank) lastMoveDstSquare _) : _)) dstSquare =
  lastMoveWasOpponentMovingPawnForTwoSquares && currentMoveIsSquareBehindLastMoveDstSquare
  where
    lastMoveWasOpponentMovingPawnForTwoSquares =
      getPiece board lastMoveSrcSquare == Just (Piece opponentColor Pawn)
        && lastMoveSrcSquareRank == startingPawnRank opponentColor
        && Just lastMoveDstSquare == (squareForward opponentColor lastMoveSrcSquare >>= squareForward opponentColor)
    currentMoveIsSquareBehindLastMoveDstSquare = squareBackward opponentColor lastMoveDstSquare == Just dstSquare
    currentPlayerColor = getCurrentPlayerColor game
    opponentColor = oppositeColor currentPlayerColor
    board = getBoard game

startingPawnRank :: Color -> Rank
startingPawnRank color = rankToPlayerRelativeRank color R2

rankToPlayerRelativeRank :: Color -> Rank -> Rank
rankToPlayerRelativeRank color rank = if color == White then rank else toEnum (7 - fromEnum rank)

getPerpendicularlyAccessibleSquares :: Color -> Board -> Square -> S.Set Square
getPerpendicularlyAccessibleSquares color =
  getAccessibleSquaresInDirections color [squareUp, squareDown, squareRight, squareLeft]

getDiagonallyAccessibleSquares :: Color -> Board -> Square -> S.Set Square
getDiagonallyAccessibleSquares color =
  getAccessibleSquaresInDirections
    color
    [ squareLeft <=< squareUp,
      squareRight <=< squareUp,
      squareLeft <=< squareDown,
      squareRight <=< squareDown
    ]

getAccessibleSquaresInDirections :: Color -> [Square -> Maybe Square] -> Board -> Square -> S.Set Square
getAccessibleSquaresInDirections color nextSquareInDirectionGetters board startSquare =
  mconcat $ (\f -> getAccessibleSquaresInDirection color f board startSquare) <$> nextSquareInDirectionGetters

getAccessibleSquaresInDirection :: Color -> (Square -> Maybe Square) -> Board -> Square -> S.Set Square
getAccessibleSquaresInDirection color getNextSquareInDirection board startSquare =
  case getNextSquareInDirection startSquare of
    Just nextSquare
      | isSquareEmpty board nextSquare ->
          S.insert nextSquare $
            getAccessibleSquaresInDirection color getNextSquareInDirection board nextSquare
      | doesSquareContainOpponentsPiece color board nextSquare -> S.singleton nextSquare
    _ -> S.empty

validate :: (a -> Bool) -> a -> Maybe a
validate p a = if p a then Just a else Nothing

onSameDiagonal :: Square -> Square -> Bool
onSameDiagonal (Square f1 r1) (Square f2 r2) = fromEnum f1 - fromEnum r1 == fromEnum f2 - fromEnum r2

onSameRank :: Square -> Square -> Bool
onSameRank (Square _ r1) (Square _ r2) = r1 == r2

onSameFile :: Square -> Square -> Bool
onSameFile (Square f1 _) (Square f2 _) = f1 == f2

getPiece :: Board -> Square -> Maybe Piece
getPiece (Board pieces) square = fst <$> find ((== square) . snd) pieces

isSquareEmpty :: Board -> Square -> Bool
isSquareEmpty board square = isNothing $ getPiece board square

doesSquareContainOpponentsPiece :: Color -> Board -> Square -> Bool
doesSquareContainOpponentsPiece color board square = case getPiece board square of
  Just piece -> pieceColor piece == color
  Nothing -> False

pieceColor :: Piece -> Color
pieceColor (Piece color _) = color

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a

safeSucc :: (Bounded a, Eq a) => a -> Maybe a
safeSucc a = if a == maxBound then Nothing else Just a

safePred :: (Bounded a, Eq a) => a -> Maybe a
safePred a = if a == minBound then Nothing else Just a

squareUp :: Square -> Maybe Square
squareUp (Square f r) = (f `Square`) <$> safeSucc r

squareDown :: Square -> Maybe Square
squareDown (Square f r) = (f `Square`) <$> safePred r

squareRight :: Square -> Maybe Square
squareRight (Square f r) = (`Square` r) <$> safeSucc f

squareLeft :: Square -> Maybe Square
squareLeft (Square f r) = (`Square` r) <$> safePred f

squareForward :: Color -> Square -> Maybe Square
squareForward White = squareUp
squareForward Black = squareDown

squareBackward :: Color -> Square -> Maybe Square
squareBackward White = squareDown
squareBackward Black = squareUp
