import qualified Data.Functor.Identity as DFI
import qualified Control.Monad.Trans.RWS.Strict as S

import qualified SequenceNumber
import qualified Data.Word as DW
import qualified Magic

data Packet a = Packet {
  packetSender    :: String,
  packetRecipient :: String,
  packetData      :: a
} deriving (Eq, Show)

data ClientMessageStart = ClientMessageStart {
  cmsMagic :: Magic.T
} deriving (Eq, Show)

data ServerMessageStart = ServerMessageStart {
  smmStartSession  :: DW.Word32,
  smmStartSequence :: SequenceNumber.T
} deriving (Eq, Show)

data ClientMessageMain a = ClientMessageMain {
  cmmSession  :: DW.Word32,
  cmmChannel  :: DW.Word8,
  cmmSequence :: SequenceNumber.T,
  cmmReliable :: Bool,
  cmmData     :: [a]
} deriving (Eq, Show)

data ServerMessageMain a = ServerMessageMain {
  smmChannel  :: DW.Word8,
  smmSequence :: SequenceNumber.T,
  smmData     :: [a]
} deriving (Eq, Show)

data ClientMessage a
  = CMStart ClientMessageStart
  | CMMain  (ClientMessageMain a)
  deriving (Eq, Show)

data ServerMessage a
  = SMStart ServerMessageStart
  | SMMain  (ServerMessageMain a)
  deriving (Eq, Show)

data CommandData = CommandData {
  cdData :: ()
} deriving (Eq, Show)

data CommandDataFragment = CommandDataFragment {
  cdfCount :: Integer,
  cdfIndex :: Integer,
  cdfData  :: ()
} deriving (Eq, Show)

data CommandPing =
  CommandPing
  deriving (Eq, Show)

data CommandPong =
  CommandPong
  deriving (Eq, Show)

data Command
  = CData CommandData
  | CDataFragment CommandDataFragment
  | CPing CommandPing
  | CPong CommandPong
  deriving (Eq, Show)

data ServerQueueEvent a
  = SQESendServerMessage (ServerMessage a)
  deriving (Eq, Show)

data ServerQueue = ServerQueue {
  sqIncomingSequence :: SequenceNumber.T
  sqOutgoingSequence :: SequenceNumber.T
} deriving (Eq, Show)

serverQueueEmpty :: ServerQueue
serverQueueEmpty = ServerQueue SequenceNumber.initial

type ServerQueueStep a =
  S.RWST () [ServerQueueEvent a] ServerQueue DFI.Identity ()

runServerQueue :: ServerQueueStep a -> ServerQueue -> (ServerQueue, [ServerQueueEvent a])
runServerQueue e s =
  case S.runRWS e () s of
    (_, s, es) -> (s, es)

serverQueueStep :: ServerQueue -> Maybe (ClientMessage a) -> (ServerQueue, [ServerQueueEvent a])
serverQueueStep q (Just (CMMain cmm)) = undefined
