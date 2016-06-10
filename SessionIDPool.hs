module SessionIDPool (T, empty, fresh) where

import qualified Data.Set as DS
import qualified SessionID

newtype T =
  SessionIDPool (DS.Set SessionID.T)
  deriving (Eq, Show)

empty :: T
empty = SessionIDPool $ DS.empty

fresh :: T -> (SessionID.T, T)
fresh (SessionIDPool pool) =
  if DS.null pool
  then
    let sid = SessionID.create 1 in
      (sid, SessionIDPool $ DS.singleton sid)
  else
    let sid = SessionID.create $ (SessionID.value $ DS.findMax pool) + 1 in
      (sid, SessionIDPool $ DS.insert sid pool)

