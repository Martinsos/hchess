{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
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

-- | Order, by user, describing a move they would like to make.
data MoveOrder = MoveOrder Square Square | PawnPromotionOrder Square Square PieceType

-- | Actual valid move that can be performed, containing some additional information about its context.
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
    capitalPieces color rank = zip (Piece color <$> capitalPiecesOrder) ((`Square` rank) <$> [FA .. FH])
    capitalPiecesOrder = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

getBoard :: Game -> Board
getBoard (Game moves) = foldl' (\board move -> fromEither $ performMoveOnBoard board move) initialBoard moves

isGameOver :: Game -> GameResult
isGameOver game = error "TODO"

isPlayerInCheck :: Color -> Board -> Bool
isPlayerInCheck currentPlayerColor board@(Board pieces) = any isKingUnderAttackByPiece oponnentPieces
  where
    kingsSquare = findKing currentPlayerColor board
    oponnentColor = oppositeColor currentPlayerColor
    oponnentPieces = filter (\(Piece c _, _) -> c == oponnentColor) pieces
    isKingUnderAttackByPiece piece = kingsSquare `S.member` getValidDstSquaresForPiece piece
    getValidDstSquaresForPiece (Piece _ _, pieceSquare) = getMoveDstSquare `S.map` fromEither (getValidSimpleMoves oponnentColor board pieceSquare)

-- | TODO: What if game is done? Do we check that here and in that case
--   don't allow performing the move? Or we don't care about that here?
performMove :: Game -> MoveOrder -> Either String Game
performMove game@(Game moves) moveOrder = do
  validMove <- makeValidMove game moveOrder
  return $ Game $ validMove : moves

-- | Performs a given move on the board, while assuming it is valid.
performMoveOnBoard :: Board -> Move -> Either String Board
performMoveOnBoard board move =
  -- TODO:
  -- 2. Move the piece from src square to dst square.
  -- 3. Remove opponents piece if it is present on the dst square.
  -- 4. Handle special cases (en passant, castlings, pawn promotion).
  error "TODO"

-- TODO: Very often I am passing around relatively boring stuff like board, currentPlayerColor, and game.
--   Maybe I should make a Reader monad that just contains Game in its Reader, since from Game we can then access all of this info?
--   And it would reduce clutter a bit? For functions that are happening in the context of current state of the game.

-- TODO: A bit wild idea: Have encoding for special kinds of moves in data, to ensure when we have them, they are valid.
--   So we would have PawnForward, PawnTwoForward, PawnEnPassant, KingsideCastlin, ... .
--   We could also have each Move contain a previous move, therefore making each move a standalone thing that can be observed on its own.
--   But maybe that is just too much complication?

-- | TODO: Consider moving Move to the separate module and then creating a smart constructor for it that ensures only valid moves can be created.
--   It would first call getValidMoves, confirm that given move (which is (Square, Squrae)) is indeed one of those valid moves, and then it would create the Move from it.
--   If we have this nice system we can even add more info to each move, like is it attack, which piece is it moving, which player, ... -> then they are very standalone which is nice.
--   Or, have this function accept more elaborate type, smth like data MoveOrder = RegularMoveOrder Square Square | PawnPromotionOrder Piece.
-- | Given current state of the game and a move order, returns an actual move that would match that move order, while ensuring it is valid.
-- If it is not a valid move, error message is returned.
makeValidMove :: Game -> MoveOrder -> Either String Move
makeValidMove game moveOrder = do
  validMoves <- getValidAndSafeMoves game $ fst $ getMoveOrderSquares moveOrder
  case find (doesMoveOrderEqualMove moveOrder) validMoves of
    Just validMove -> Right validMove
    Nothing -> Left "Can't move there"
  where
    -- TODO: move more global?
    getMoveOrderSquares (MoveOrder src dst) = (src, dst)
    getMoveOrderSquares (PawnPromotionOrder src dst _) = (src, dst)

    -- TODO: move more global?
    doesMoveOrderEqualMove moveOrder' (Move srcSquare dstSquare moveType) =
      case moveOrder' of
        MoveOrder srcSquare' dstSquare' -> srcSquare == srcSquare' && dstSquare == dstSquare'
        PawnPromotionOrder srcSquare' dstSquare' newPieceType' -> srcSquare == srcSquare' && dstSquare == dstSquare' && moveType == PawnPromotion newPieceType'

-- TODO: This function is huge, take it out into separate module and move complex functions in @where@ to standalone functions.

-- TODO: Rename "valid" to "possible" and "validAndSafe" to "valid"? What I am trying to do is find a name
--   for moves that are valid but might expose the king to check, and then also find a name for moves
--   that are valid but also don't expose the king to a check.

getValidAndSafeMoves :: Game -> Square -> Either String (S.Set Move)
getValidAndSafeMoves game srcSquare = do
  validMoves <- getValidMoves game srcSquare
  return $ S.filter (not . doesMovePutOwnKingInCheck) validMoves
  where
    (board, currentPlayerColor) = (getBoard game, getCurrentPlayerColor game)

    doesMovePutOwnKingInCheck :: Move -> Bool
    doesMovePutOwnKingInCheck move = isPlayerInCheck currentPlayerColor (fromEither $ performMoveOnBoard board move)

-- NOTE: This returns all moves including for castling and en passant. It doesn't check if a move exposes its own king to a check.
getValidMoves :: Game -> Square -> Either String (S.Set Move)
getValidMoves game srcSquare = do
  -- TODO: This is duplicated in getValidSimpleMoves.
  (Piece srcPieceColor srcPieceType) <- maybeToEither "No piece at specified location" $ getPiece board srcSquare
  when (srcPieceColor /= currentPlayerColor) $ Left "Can't move oponnent's piece"

  simpleMoves <- getValidSimpleMoves currentPlayerColor board srcSquare

  -- NOTE: special moves are castling and enpassant. They are special because they require history of the game, not just current board state.
  -- Also, interesting and important -> none of them can attack enemy king.
  -- TODO: Extract this into special getValidSpecialMoves function?
  let specialMoves = case srcPieceType of
        Pawn -> pawnValidSpecialMoves
        King -> kingValidSpecialMoves
        _ -> S.empty

  return $ simpleMoves `S.union` specialMoves
  where
    (board, currentPlayerColor) = (getBoard game, getCurrentPlayerColor game)

    pawnValidSpecialMoves =
      (S.fromList . catMaybes)
        [ squareFwd srcSquare >>= squareRight >>= registerAsEnPassant,
          squareFwd srcSquare >>= squareLeft >>= registerAsEnPassant
        ]
      where
        -- TODO: Duplication.
        squareFwd = squareForward currentPlayerColor

        registerAsEnPassant :: Square -> Maybe Move
        registerAsEnPassant dstSquare
          | isMoveEnPassant game dstSquare = return $ mkMove EnPassant dstSquare
          | otherwise = Nothing

        -- TODO: Duplication.
        mkMove :: MoveType -> Square -> Move
        mkMove moveType dstSquare = Move srcSquare dstSquare moveType

    kingValidSpecialMoves = error "TODO: castling"

-- NOTE: This returns all moves except for castling and en passant. Also, it doesn't check if a move exposes its own king to a check.
getValidSimpleMoves :: Color -> Board -> Square -> Either String (S.Set Move)
getValidSimpleMoves currentPlayerColor board src@(Square _ srcRank) = do
  (Piece srcPieceColor srcPieceType) <- maybeToEither "No piece at specified location" $ getPiece board src
  when (srcPieceColor /= currentPlayerColor) $ Left "Can't move oponnent's piece"

  let validMoves = case srcPieceType of
        Pawn -> pawnValidMoves
        Knight -> knightValidMoves
        Bishop -> bishopValidMoves
        Rook -> rookValidMoves
        Queen -> queenValidMoves
        King -> kingValidMoves
  return validMoves
  where
    mkMove :: MoveType -> Square -> Move
    mkMove moveType dstSquare = Move src dstSquare moveType

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
