module Model where

import qualified Data.Functor.Identity as DFI
import qualified Control.Monad.Trans.RWS.Strict as S
import qualified Data.Map.Strict as DM

import qualified Magic
import qualified Address
import qualified SessionID
import qualified SessionIDPool
import qualified SequenceNumber

data ClientMessageSetup =
  ClientMessageSetup Magic.T
  deriving (Eq, Show)

data ClientMessage
  = CMSetup ClientMessageSetup
  | CMMain
  deriving (Eq, Show)

msgSetup :: Magic.T -> ClientMessage
msgSetup = CMSetup . ClientMessageSetup

data Event
  = EventIgnoredGarbage Address.T
  | EventIgnoredWrongMagic Address.T Magic.T
  | EventClientConnected Address.T SessionID.T
  | EventSendServerPacket ServerPacket
  deriving (Eq, Show)

data ClientPacketData
  = PCMessage ClientMessage
  | PCGarbage
  deriving (Eq, Show)

data ClientPacket = ClientPacket {
  clientPacketSender :: Address.T,
  clientPacketData   :: ClientPacketData
} deriving (Eq, Show)

clientPacketSetup :: Address.T -> Magic.T -> ClientPacket
clientPacketSetup sender = (ClientPacket sender) . PCMessage . msgSetup

data ServerMessageNewSession = ServerMessageNewSession {
  smnsSessionID :: SessionID.T,
  smnsSequenceNumber :: SequenceNumber.T
} deriving (Eq, Show)

data ServerPacket
  = SPNewSession Address.T ServerMessageNewSession
  deriving (Eq, Show)

serverPacketNewSession :: Address.T -> SessionID.T -> ServerPacket
serverPacketNewSession address session_id =
  SPNewSession address (ServerMessageNewSession session_id SequenceNumber.initial)

data Session = Session {
  sessionID :: SessionID.T
} deriving (Eq, Show)

data Server = Server {
  serverIDPool   :: SessionIDPool.T,
  serverSessions :: DM.Map SessionID.T Session,
  serverMagic    :: Magic.T
} deriving (Eq, Show)

serverEmpty :: Server
serverEmpty = Server {
  serverIDPool   = SessionIDPool.empty,
  serverSessions = DM.empty,
  serverMagic    = Magic.create 0xCAFECAFE
}

type ServerStep =
  S.RWST () [Event] Server DFI.Identity ()

serverRun :: ServerStep -> Server -> (Server, [Event])
serverRun e s =
  case S.runRWS e () s of
    (_, s, es) -> (s, es)

serverEvent :: Event -> ServerStep
serverEvent e = S.tell [e]

serverCreateSession :: Address.T -> ServerStep
serverCreateSession client_address =
  do {
    server <- S.get;
    let (session_id, pool) = SessionIDPool.fresh (serverIDPool server) in
    let session = Session session_id in
    let sessions = DM.insert session_id session (serverSessions server) in
    do {
      S.put (server {
        serverIDPool = pool,
        serverSessions = sessions
      });
      serverEvent $ EventClientConnected client_address session_id;
      serverEvent $ EventSendServerPacket $ serverPacketNewSession client_address session_id;
    }
  }

serverClientPacketProcess :: ClientPacket -> ServerStep
serverClientPacketProcess (ClientPacket sender PCGarbage) =
  do serverEvent $ EventIgnoredGarbage sender;
     return ()

serverClientPacketProcess (ClientPacket sender (PCMessage (CMSetup (ClientMessageSetup magic)))) =
  do {
    server <- S.get;
    if serverMagic server /= magic
    then serverEvent (EventIgnoredWrongMagic sender magic) >> return ()
    else serverCreateSession sender
  }

