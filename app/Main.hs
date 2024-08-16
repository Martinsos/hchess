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

-- TODO: Consider refactoring MoveOrder and Move to be better. Make them more similar? Have just Move?
--   MoveOrder seems maybe a bit too much like special case.
--   We could at least make it data MoveOrder = MoverOrder Square Square MoveOrderType, so it is more similar to Move.
--   But then can we have just Move? But we did have reasons for having just MoveOrder.
--   What if we had data Move = RegularMove Square Square | EnPassant Square | KingsideCastling | QueensideCastling | PawnPromotion Square PieceType
--     and then also data MoveOrder = RegularMoveOrder Square Square | PawnPromotionOrder Square PieceType ? (if we do need MoveOrder).
--   ...
--   Ok, so thinking more about this and looking at the code below, I think we might really not need MoveOrder.
--   I was assuming that from somewhere outside (the player) we will have these move orders coming in these raw shape.
--   But what is going to be more natural is for the player/game to ask for valid moves, and then pick one of those.
--   And for that, we need only Move! So I don't think we need MoveOrder at all -> I created it only because I was trying to guess
--   what will be needed and imagined it is needed, but it seems to me that was incorrect now and it is not needed at all.
--   Which is great!

-- | Order, by user, describing a move they would like to make.
data MoveOrder = MoveOrder Square Square | PawnPromotionOrder Square Square PieceType

-- | Actual valid move that can be performed, containing some additional information about its context.
data Move = Move Square Square MoveType
  deriving (Eq, Ord)

-- TODO: Consider embedding more info into these, to make it easier to analyze them
--   standalone to some degree, without having to calculate the whole board.
--   This might be especially useful for checking conditions for game over.
data MoveType = RegularMove | EnPassant | KingsideCastling | QueensideCastling | PawnPromotion PieceType
  deriving (Eq, Show, Ord)

data Square = Square File Rank
  deriving (Eq, Ord)

-- TODO: Just make Square a record instead of having these functions here?
squareFile :: Square -> File
squareFile (Square file _) = file

squareRank :: Square -> Rank
squareRank (Square _ rank) = rank

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
getBoard (Game moves) = foldl' performValidMoveOnBoard initialBoard $ reverse moves

checkIfGameOver :: Game -> Maybe GameResult
checkIfGameOver game
  | playerHasNoValidAndSafeMoves currentPlayerColor =
      if isPlayerInCheck currentPlayerColor board
        then Just $ Victory $ oppositeColor currentPlayerColor
        else Just Draw
  | noHappeningsIn50Moves = Just Draw
  | insufficientMaterial = Just Draw
  | otherwise = Nothing
  where
    (board, currentPlayerColor) = (getBoard game, getCurrentPlayerColor game)

    playerHasNoValidAndSafeMoves :: Color -> Bool
    playerHasNoValidAndSafeMoves playerColor =
      null (mconcat $ map (fromEither . getValidAndSafeMoves game) (squaresWithPiecesOfColor playerColor))

    squaresWithPiecesOfColor :: Color -> [Square]
    squaresWithPiecesOfColor color =
      let (Board pieces) = board in snd <$> filter (\((Piece c _), _) -> c == color) pieces

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

isPlayerInCheck :: Color -> Board -> Bool
isPlayerInCheck currentPlayerColor board@(Board pieces) = any isKingUnderAttackByPiece oponnentPieces
  where
    kingsSquare = findKing currentPlayerColor board
    oponnentColor = oppositeColor currentPlayerColor
    oponnentPieces = filter (\(Piece c _, _) -> c == oponnentColor) pieces
    isKingUnderAttackByPiece piece = kingsSquare `S.member` getValidDstSquaresForPiece piece
    getValidDstSquaresForPiece (Piece _ _, pieceSquare) =
      getMoveDstSquare `S.map` fromEither (getValidSimpleMoves oponnentColor board pieceSquare)

-- TODO: Is String here error? Then use type Error = String.

-- | TODO: What if game is done? Do we check that here and in that case
--   don't allow performing the move? Or we don't care about that here?
--   I think we do that outside of here, and don't care about it here.
performMove :: Game -> MoveOrder -> Either String Game
performMove game@(Game moves) moveOrder = do
  validMove <- makeValidMove game moveOrder
  return $ Game $ validMove : moves

-- | Performs a given move on the board, while assuming it is valid.
performValidMoveOnBoard :: Board -> Move -> Board
performValidMoveOnBoard board (Move src dst moveType) =
  case moveType of
    RegularMove -> movePieceFromTo src dst . removeAnyPieceAt dst $ board
    EnPassant -> movePieceFromTo src dst . removeAnyPieceAt (Square (squareFile dst) (squareRank src)) $ board
    KingsideCastling ->
      let (rookSrcSquare, rookDstSquare) = (Square FH $ squareRank src, Square FF $ squareRank src)
       in movePieceFromTo src dst . movePieceFromTo rookSrcSquare rookDstSquare $ board
    QueensideCastling ->
      let (rookSrcSquare, rookDstSquare) = (Square FA $ squareRank src, Square FD $ squareRank src)
       in movePieceFromTo src dst . movePieceFromTo rookSrcSquare rookDstSquare $ board
    PawnPromotion newPieceType ->
      let (Piece color _) = fromJust $ getPieceAt src board
       in putPieceAt dst (Piece color newPieceType) . removeAnyPieceAt src $ board
  where
    movePieceFromTo :: Square -> Square -> Board -> Board
    movePieceFromTo src' dst' (Board pieces) =
      Board $ (fmap . fmap) (\sq -> if sq == src' then dst' else sq) pieces

    removeAnyPieceAt :: Square -> Board -> Board
    removeAnyPieceAt square (Board pieces) = Board $ filter ((/= square) . snd) pieces

    putPieceAt :: Square -> Piece -> Board -> Board
    putPieceAt sq piece (Board pieces) = Board $ (piece, sq) : pieces

    getPieceAt :: Square -> Board -> Maybe Piece
    getPieceAt sq (Board pieces) = fst <$> find ((== sq) . snd) pieces

-- TODO: Very often I am passing around relatively boring stuff like board, currentPlayerColor, and game.
--   Maybe I should make a Reader monad that just contains Game in its Reader, since from Game we can then access all of this info?
--   And it would reduce clutter a bit? For functions that are happening in the context of current state of the game.

-- TODO: A bit wild idea: Have encoding for special kinds of moves in data, to ensure when we have them, they are valid.
--   So we would have PawnForward, PawnTwoForward, PawnEnPassant, KingsideCastlin, ... .
--   We could also have each Move contain a previous move, therefore making each move a standalone thing that can be observed on its own.
--   But maybe that is just too much complication?
--   data Game = Game { lastMove :: Move, gameBeforeLastMove :: Maybe Game }

-- | TODO: Consider moving Move to the separate module and then creating a smart constructor for it that ensures only valid moves can be created.
--   It would first call getValidMoves, confirm that given move (which is (Square, Squrae)) is indeed one of those valid moves, and then it would create the Move from it.
--   If we have this nice system we can even add more info to each move, like is it attack, which piece is it moving, which player, ... -> then they are very standalone which is nice.
--   Or, have this function accept more elaborate type, smth like data MoveOrder = RegularMoveOrder Square Square | PawnPromotionOrder Piece.
--
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
--   Or maybe I should drop the idea of having two names for this stuff and just go with "valid" moves,
--   which are moves that are both ok and don't put king in danger? getValidAndSafeMoves doesn't do much
--   at the moment anyway, I could just move its logic into getValidMoves? I can define helper function
--   that filter outs moves that endanger king, and just call that in the getValidMoves and there we go.
--   Ah but that is an issue because I have helper functions that look for valid but unsafe moves so how
--   do I call them? Maybe this will be a bit easier once I extract it into a separate module.
--   Maybe "valid" vs "potentially valid"?
--   Yeah, I should probably just call "validAndSafe" move "valid" moves, drop the concept of unsafe ones,
--   and push logic from `getValidAndSafeMoves` into `getValidMoves`.

getValidAndSafeMoves :: Game -> Square -> Either String (S.Set Move)
getValidAndSafeMoves game srcSquare = do
  validMoves <- getValidMoves game srcSquare
  return $ S.filter (not . doesMovePutOwnKingInCheck) validMoves
  where
    (board, currentPlayerColor) = (getBoard game, getCurrentPlayerColor game)

    doesMovePutOwnKingInCheck :: Move -> Bool
    doesMovePutOwnKingInCheck move = isPlayerInCheck currentPlayerColor (fromEither $ performValidMoveOnBoard board move)

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
        [ squareFwd srcSquare >>= squareRight >>= makeAnEnPassantMove,
          squareFwd srcSquare >>= squareLeft >>= makeAnEnPassantMove
        ]
      where
        -- TODO: Duplication.
        squareFwd = squareForward currentPlayerColor

        makeAnEnPassantMove :: Square -> Maybe Move
        makeAnEnPassantMove dstSquare
          | isMoveEnPassant game dstSquare = return $ mkMove EnPassant dstSquare
          | otherwise = Nothing

        -- TODO: Duplication.
        mkMove :: MoveType -> Square -> Move
        mkMove moveType dstSquare = Move srcSquare dstSquare moveType

    kingValidSpecialMoves =
      -- TODO:
      --  1. Check that king was never moved.
      --  2. Check that rook has never moved.
      --  3. Check that square between the kind and rook are vacant.
      --  4. Neither king, rook, or any square in between them is under attack.
      --  We need to check this for both kingside castling and for queenside castling.
      --  To check if king has never moved, it is best to just check if there was ever a move that had its initial square as src.
      --  To check if rook has never moved, we can do this same check, although it might have in theory been moved by castling,
      --    but in that case the check with "did king move" will fail first anyway.
      --  Checking that squares in between are vacant should be trivial.
      --  Checking that none of the squares is under attack: I should take a look at `isPlayerInCheck` function,
      --  refactor it into more general `isSquareUnderAttack` function, and then use that function here plus
      --  also redefine isPlayerInCheck via it.
      error "TODO: castling"

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
-- We assume that move is valid, in a sense that there is such pawn, owned by the player that currently has turn,
-- that can be moved to this square.
-- TODO: This feels pretty much like a helper function, maybe move it somewhere to make it less global?
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

-- TODO: Define (in comment) what accessible means (and what this function does).
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

safeSucc :: (Bounded a, Eq a, Enum a) => a -> Maybe a
safeSucc a = if a == maxBound then Nothing else Just $ succ a

safePred :: (Bounded a, Eq a, Enum a) => a -> Maybe a
safePred a = if a == minBound then Nothing else Just $ pred a

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
