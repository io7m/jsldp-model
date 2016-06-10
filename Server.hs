module Server where

import qualified Data.Functor.Identity as DFI
import qualified Control.Monad.Trans.RWS.Strict as S
import qualified Data.Map.Strict as DM

import qualified Address
import qualified ClientMessage
import qualified Magic
import qualified Packet
import qualified SequenceNumber
import qualified ServerMessage
import qualified SessionID
import qualified SessionIDPool

data Event
  = EventIgnoredGarbage Address.T
  | EventIgnoredWrongMagic Address.T Magic.T
  | EventClientConnected Address.T SessionID.T
  | EventSendServerPacket (Packet.T ServerMessage.T)
  deriving (Eq, Show)

packetNewSession :: Address.T -> Address.T -> SessionID.T -> Packet.T ServerMessage.T
packetNewSession sender recipient session_id =
  Packet.packet sender recipient $ ServerMessage.SMNewSession $ ServerMessage.NewSession session_id SequenceNumber.initial

data Session = Session {
  sessionID :: SessionID.T
} deriving (Eq, Show)

data Config = Config {
  configMagic   :: Magic.T,
  configAddress :: Address.T
} deriving (Eq, Show)

data T = T {
  idPool   :: SessionIDPool.T,
  sessions :: DM.Map SessionID.T Session
} deriving (Eq, Show)

empty :: T
empty = T {
  idPool   = SessionIDPool.empty,
  sessions = DM.empty
}

type ServerStep =
  S.RWST Config [Event] T DFI.Identity ()

run :: ServerStep -> Config -> T -> (T, [Event])
run e c s =
  case S.runRWS e c s of
    (_, s, es) -> (s, es)

event :: Event -> ServerStep
event e = S.tell [e]

createSession :: Address.T -> ServerStep
createSession client_address =
  do {
    server <- S.get;
    config <- S.ask;
    let (session_id, pool) = SessionIDPool.fresh (idPool server) in
    let session = Session session_id in
    let sessions' = DM.insert session_id session (sessions server) in
    do {
      S.put (server {
        idPool = pool,
        sessions = sessions'
      });
      event $ EventClientConnected client_address session_id;
      event $ EventSendServerPacket $ packetNewSession (configAddress config) client_address session_id;
    }
  }

processClientPacket :: Packet.T ClientMessage.T -> ServerStep
processClientPacket (Packet.T sender _ Packet.DGarbage) =
  do event $ EventIgnoredGarbage sender;
     return ()

processClientPacket
  (Packet.T sender _ (Packet.DMessage (ClientMessage.CMSetup (ClientMessage.Setup magic)))) =
  do {
    config <- S.ask;
    if configMagic config /= magic
    then event (EventIgnoredWrongMagic sender magic) >> return ()
    else createSession sender
  }

