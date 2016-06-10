module ServerMessage where

import qualified SessionID
import qualified SequenceNumber

data NewSession = NewSession {
  nsSessionID      :: SessionID.T,
  nsSequenceNumber :: SequenceNumber.T
} deriving (Eq, Show)

data T
  = SMNewSession NewSession
  deriving (Eq, Show)

