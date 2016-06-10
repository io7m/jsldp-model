module Packet where

import qualified Address

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
packet sender recipient x = T sender recipient $ DMessage x

garbage :: Address.T -> Address.T -> T a
garbage sender recipient = T sender recipient $ DGarbage

