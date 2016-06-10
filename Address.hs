module Address (
  T,
  create,
  exampleSender0
) where

data T =
  Address String Integer
  deriving (Eq, Show)

create :: String -> Integer -> T
create host port = Address host port

exampleSender0 :: T
exampleSender0 = create "127.0.0.1" 10000

