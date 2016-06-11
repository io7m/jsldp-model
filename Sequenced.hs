module Sequenced where

import qualified SequenceNumber

class Sequenced a where
  sequence :: a -> SequenceNumber.T

