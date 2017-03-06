module WebSocket.RPC.Types where

import Prelude
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (.?), (:=), (~>), jsonEmptyObject)
import Data.Generic (class Generic, gShow, gEq, gCompare)
import Data.Enum (class Enum, pred, succ)


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
