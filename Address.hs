module Address (
  T,
  create
) where

data T =
  Address String Integer
  deriving (Eq, Show)

create :: String -> Integer -> T
create host port = Address host port

