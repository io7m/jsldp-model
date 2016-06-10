module Magic (T, create) where

newtype T =
  Magic Integer
  deriving (Eq, Show)

create :: Integer -> T
create = Magic

