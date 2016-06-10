module SequenceNumber (T, initial, next, distance, from) where

import qualified Data.Bits as DB
import qualified Data.Word as DW
import qualified Data.Int  as DI

import Data.Bits ((.|.))

newtype T =
  SN DW.Word16
  deriving Show

from :: Integer -> T
from = SN . fromInteger

initial :: T
initial = from 0

next :: T -> T
next (SN x) = SN $ x + (fromInteger 1)

distance :: T -> T -> Integer
distance (SN x) (SN y) =
  let z = y - x
      s :: DI.Int16
      s = fromInteger (toInteger z) in
    toInteger s

instance Eq T where
  (==) x y = distance x y == 0

