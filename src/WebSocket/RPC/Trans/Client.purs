module WebSocket.RPC.Trans.Client where

import Prelude
import Data.Map as Map
import Control.Monad.State.Trans (StateT, evalStateT, runStateT, get, put)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Enum (succ)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import WebSocket.RPC.Types (RPCID)



type Conts rep com m =
  { reply    :: rep -> m Unit
  , complete :: com -> m Unit
  }


type State rep com m =
  { rpcIdent :: RPCID
  , rpcConts :: Map RPCID (Conts rep com m)
  }


newState :: forall rep com m. State rep com m
newState =
  { rpcIdent : bottom
  , rpcConts : Map.empty
  }


newtype WebSocketClientRPCT rep com m a = WebSocketClientRPCT (StateT (State rep com m) m a)

runWebSocketClientRPCT :: forall rep com m a. WebSocketClientRPCT rep com m a -> StateT (State rep com m) m a
runWebSocketClientRPCT (WebSocketClientRPCT x) = x

runWebSocketClientRPCT' :: forall rep com m a
                        . State rep com m -> WebSocketClientRPCT rep com m a -> m (Tuple a (State rep com m))
runWebSocketClientRPCT' s x = runStateT (runWebSocketClientRPCT x) s

getClientState :: forall rep com m
                . Monad m => WebSocketClientRPCT rep com m (State rep com m)
getClientState = WebSocketClientRPCT get

execWebSocketClientRPCT :: forall rep com m a. Functor m => WebSocketClientRPCT rep com m a -> m a
execWebSocketClientRPCT (WebSocketClientRPCT x) = evalStateT x newState

instance functorWebSocketClientRPCT :: Functor m => Functor (WebSocketClientRPCT rep com m) where
  map f (WebSocketClientRPCT x) = WebSocketClientRPCT (map f x)

instance applyWebSocketClientRPCT :: Monad m => Apply (WebSocketClientRPCT rep com m) where
  apply (WebSocketClientRPCT f) (WebSocketClientRPCT x) = WebSocketClientRPCT (apply f x)

instance applicativeWebSocketClientRPCT :: Monad m => Applicative (WebSocketClientRPCT rep com m) where
  pure x = WebSocketClientRPCT (pure x)

instance bindWebSocketClientRPCT :: Monad m => Bind (WebSocketClientRPCT rep com m) where
  bind (WebSocketClientRPCT x) f = WebSocketClientRPCT (bind x (runWebSocketClientRPCT <<< f))

instance monadWebSocketClientRPCT :: Monad m => Monad (WebSocketClientRPCT rep com m)

instance monadTransWebSocketClientRPCT :: MonadTrans (WebSocketClientRPCT rep com) where
  lift x = WebSocketClientRPCT (lift x)


freshRPCID :: forall rep com m. Monad m => WebSocketClientRPCT rep com m RPCID
freshRPCID = WebSocketClientRPCT do
  {rpcIdent, rpcConts} <- get
  let rpcIdent' = case succ rpcIdent of
                    Nothing -> bottom
                    Just x  -> x
  put {rpcIdent: rpcIdent', rpcConts}
  pure rpcIdent


registerReplyComplete :: forall rep com m. Monad m => RPCID -> (rep -> m Unit) -> (com -> m Unit) -> WebSocketClientRPCT rep com m Unit
registerReplyComplete rpcId reply complete = WebSocketClientRPCT do
  {rpcIdent, rpcConts} <- get
  put {rpcIdent, rpcConts: Map.insert rpcId {reply,complete} rpcConts}



unregisterReplyComplete :: forall rep com m. Monad m => RPCID -> WebSocketClientRPCT rep com m Unit
unregisterReplyComplete rpcId = WebSocketClientRPCT do
  {rpcIdent, rpcConts} <- get
  put {rpcIdent, rpcConts: Map.delete rpcId rpcConts}


runReply :: forall rep com m. Monad m => RPCID -> rep -> WebSocketClientRPCT rep com m Unit
runReply rpcId rep = WebSocketClientRPCT do
  {rpcConts} <- get
  case Map.lookup rpcId rpcConts of
    Nothing -> pure unit -- TODO throw warning?
    Just {reply} -> lift (reply rep)


runComplete :: forall rep com m. Monad m => RPCID -> com -> WebSocketClientRPCT rep com m Unit
runComplete rpcId com = WebSocketClientRPCT do
  {rpcConts} <- get
  case Map.lookup rpcId rpcConts of
    Nothing -> pure unit -- TODO throw warning?
    Just {complete} -> lift (complete com)
