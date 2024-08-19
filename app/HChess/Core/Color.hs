module HChess.Core.Color
  ( Color (..),
    oppositeColor,
  )
where

data Color = White | Black
  deriving (Eq, Show)

oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White
