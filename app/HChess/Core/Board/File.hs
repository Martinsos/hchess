module HChess.Core.Board.File
  ( File (..),
  )
where

import Data.Char (chr, ord)

data File = FA | FB | FC | FD | FE | FF | FG | FH
  deriving (Eq, Ord, Enum, Bounded)

instance Show File where
  show = pure . chr . (ord 'a' +) . fromEnum
