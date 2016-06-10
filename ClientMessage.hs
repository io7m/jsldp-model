module ClientMessage where

import qualified Magic
import qualified SessionID
import qualified SequenceNumber

data Setup =
  Setup Magic.T
  deriving (Eq, Show)

data T
  = CMSetup Setup
  | CMMain
  deriving (Eq, Show)

setup :: Magic.T -> T
setup = CMSetup . Setup
