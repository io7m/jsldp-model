module ClientCommand where

import qualified Magic
import qualified SessionID
import qualified SequenceNumber
import qualified Sequenced as S

data Hello =
  Hello Magic.T
  deriving (Eq, Show)

data Initial =
  IHello Hello
  deriving (Eq, Show)

data UnreliableData = UnreliableData {
  udSequence :: SequenceNumber.T,
  udData     :: ()
} deriving (Eq, Show)

instance S.Sequenced UnreliableData where
  sequence = udSequence

data Main
  = MUnreliableData UnreliableData
  deriving (Eq, Show)

instance S.Sequenced Main where
  sequence (MUnreliableData u) = S.sequence u

data T
  = CInitial Initial
  | CMain Main
  deriving (Eq, Show)

