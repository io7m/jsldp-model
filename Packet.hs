module Packet where

import qualified Address
import qualified SequenceNumber

data Data a
  = DMessage a
  | DGarbage
  deriving (Eq, Show)

data T a = T {
  packetSender    :: Address.T,
  packetRecipient :: Address.T,
  packetData      :: Data a
} deriving (Eq, Show)

packet :: Address.T -> Address.T -> a -> T a
packet sender recip x = T sender recip $ DMessage x

garbage :: Address.T -> Address.T -> T a
garbage sender recip = T sender recip $ DGarbage

