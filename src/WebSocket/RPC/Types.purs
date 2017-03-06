module WebSocket.RPC.Types where

import Prelude
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (.?), (:=), (~>), jsonEmptyObject)
import Data.Generic (class Generic, gShow, gEq, gCompare)
import Data.Enum (class Enum, pred, succ)
import Data.Maybe (Maybe)


newtype RPCID = RPCID Int

derive instance genericRPCID :: Generic RPCID

instance showRPCID :: Show RPCID where
  show = gShow

instance eqRPCID :: Eq RPCID where
  eq = gEq

instance ordRPCID :: Ord RPCID where
  compare = gCompare

instance boundedRPCID :: Bounded RPCID where
  bottom = RPCID bottom
  top = RPCID top

instance enumRPCID :: Enum RPCID where
  pred (RPCID x) = RPCID <$> pred x
  succ (RPCID x) = RPCID <$> succ x

instance encodeJsonRPCID :: EncodeJson RPCID where
  encodeJson (RPCID x) = encodeJson x

instance decodeJsonRPCID :: DecodeJson RPCID where
  decodeJson json = RPCID <$> decodeJson json


newtype RPCIdentified a = RPCIdentified
  { _ident  :: RPCID
  , _params :: a
  }

derive instance genericRPCIdentified :: Generic a => Generic (RPCIdentified a)

instance eqRPCIdentified :: (Eq a, Generic a) => Eq (RPCIdentified a) where
  eq = gEq

instance showRPCIdentified :: (Show a, Generic a) => Show (RPCIdentified a) where
  show = gShow

instance encodeJsonRPCIdentified :: (EncodeJson a) => EncodeJson (RPCIdentified a) where
  encodeJson (RPCIdentified {_ident, _params})
    =  "ident" := encodeJson _ident
    ~> "params" := encodeJson _params
    ~> jsonEmptyObject

instance decodeJsonRPCIdentified :: (DecodeJson a) => DecodeJson (RPCIdentified a) where
  decodeJson json = do
    o <- decodeJson json
    _ident <- o .? "ident"
    _params <- o .? "params"
    pure (RPCIdentified {_ident, _params})


newtype Subscribe a = Subscribe (RPCIdentified a)

derive instance genericSubscribe :: Generic a => Generic (Subscribe a)

instance eqSubscribe :: (Eq a, Generic a) => Eq (Subscribe a) where
  eq = gEq

instance showSubscribe :: (Show a, Generic a) => Show (Subscribe a) where
  show = gShow

instance encodeJsonSubscribe :: (EncodeJson a) => EncodeJson (Subscribe a) where
  encodeJson (Subscribe x) = encodeJson x

instance decodeJsonSubscribe :: (DecodeJson a) => DecodeJson (Subscribe a) where
  decodeJson = map Subscribe <<< decodeJson


newtype Supply a = Supply (RPCIdentified (Maybe a))

derive instance genericSupply :: Generic a => Generic (Supply a)

instance eqSupply :: (Eq a, Generic a) => Eq (Supply a) where
  eq = gEq

instance showSupply :: (Show a, Generic a) => Show (Supply a) where
  show = gShow

instance encodeJsonSupply :: (EncodeJson a) => EncodeJson (Supply a) where
  encodeJson (Supply x) = encodeJson x

instance decodeJsonSupply :: (DecodeJson a) => DecodeJson (Supply a) where
  decodeJson = map Supply <<< decodeJson


newtype Reply a = Reply (RPCIdentified a)

derive instance genericReply :: Generic a => Generic (Reply a)

instance eqReply :: (Eq a, Generic a) => Eq (Reply a) where
  eq = gEq

instance showReply :: (Show a, Generic a) => Show (Reply a) where
  show = gShow

instance encodeJsonReply :: (EncodeJson a) => EncodeJson (Reply a) where
  encodeJson (Reply x) = encodeJson x

instance decodeJsonReply :: (DecodeJson a) => DecodeJson (Reply a) where
  decodeJson = map Reply <<< decodeJson


newtype Complete a = Complete (RPCIdentified a)

derive instance genericComplete :: Generic a => Generic (Complete a)

instance eqComplete :: (Eq a, Generic a) => Eq (Complete a) where
  eq = gEq

instance showComplete :: (Show a, Generic a) => Show (Complete a) where
  show = gShow

instance encodeJsonComplete :: (EncodeJson a) => EncodeJson (Complete a) where
  encodeJson (Complete x) = encodeJson x

instance decodeJsonComplete :: (DecodeJson a) => DecodeJson (Complete a) where
  decodeJson = map Complete <<< decodeJson
