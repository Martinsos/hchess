module HChess.Utils
  ( fromEither,
    validate,
    maybeToEither,
    safeSucc,
    safePred,
  )
where

fromEither :: Either a b -> b
fromEither (Right x) = x
fromEither (Left _) = error "Encountered Left, but expected Right"

validate :: (a -> Bool) -> a -> Maybe a
validate p a = if p a then Just a else Nothing

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a

safeSucc :: (Bounded a, Eq a, Enum a) => a -> Maybe a
safeSucc a = if a == maxBound then Nothing else Just $ succ a

safePred :: (Bounded a, Eq a, Enum a) => a -> Maybe a
safePred a = if a == minBound then Nothing else Just $ pred a
