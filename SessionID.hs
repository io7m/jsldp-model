module SessionID (T, create, value) where

newtype T =
  SessionID Integer
  deriving (Eq, Ord, Show)

create :: Integer -> T
create x = SessionID x

value :: T -> Integer
value (SessionID x) = x

