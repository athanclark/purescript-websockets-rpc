module WebSocket.RPC.Trans.Client
  ( Conts, Env, WebSocketClientRPCT, runWebSocketClientRPCT', getClientEnv, execWebSocketClientRPCT
  , freshRPCID, registerReplyComplete, unregisterReplyComplete, runReply, runComplete
  ) where

import Prelude
import Data.Map as Map
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Ref (Ref, REF, newRef, readRef, writeRef)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Enum (succ)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import WebSocket.RPC.Types (RPCID)



type Conts rep com m =
  { reply    :: rep -> m Unit
  , complete :: com -> m Unit
  }


type Env rep com m =
  { rpcIdentRef :: Ref RPCID
  , rpcContsRef :: Ref (Map RPCID (Conts rep com m))
  }


newEnv :: forall rep com m eff. Eff (ref :: REF | eff) (Env rep com m)
newEnv = do
  rpcIdentRef <- newRef bottom
  rpcContsRef <- newRef Map.empty
  pure {rpcIdentRef,rpcContsRef}


newtype WebSocketClientRPCT rep com m a = WebSocketClientRPCT (ReaderT (Env rep com m) m a)

runWebSocketClientRPCT :: forall rep com m a. WebSocketClientRPCT rep com m a -> ReaderT (Env rep com m) m a
runWebSocketClientRPCT (WebSocketClientRPCT x) = x

runWebSocketClientRPCT' :: forall rep com m a. Env rep com m -> WebSocketClientRPCT rep com m a -> m a
runWebSocketClientRPCT' env x = runReaderT (runWebSocketClientRPCT x) env

getClientEnv :: forall rep com m. Monad m => WebSocketClientRPCT rep com m (Env rep com m)
getClientEnv = WebSocketClientRPCT ask

execWebSocketClientRPCT :: forall rep com m a eff
                         . MonadEff (ref :: REF | eff) m => WebSocketClientRPCT rep com m a -> m a
execWebSocketClientRPCT x = do
  env <- liftEff newEnv
  runWebSocketClientRPCT' env x

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

instance monadEffWebSocketClientRPCT :: MonadEff eff m => MonadEff eff (WebSocketClientRPCT rep com m) where
  liftEff x = WebSocketClientRPCT (liftEff x)


freshRPCID :: forall rep com m eff
            . MonadEff (ref :: REF | eff) m => WebSocketClientRPCT rep com m RPCID
freshRPCID = WebSocketClientRPCT do
  {rpcIdentRef} <- ask
  rpcIdent <- liftEff (readRef rpcIdentRef)
  let rpcIdent' = case succ rpcIdent of
                    Nothing -> bottom
                    Just x  -> x
  liftEff (writeRef rpcIdentRef rpcIdent')
  pure rpcIdent


registerReplyComplete :: forall rep com m eff
                       . MonadEff (ref :: REF | eff) m => RPCID -> (rep -> m Unit) -> (com -> m Unit) -> WebSocketClientRPCT rep com m Unit
registerReplyComplete rpcId reply complete = WebSocketClientRPCT do
  {rpcContsRef} <- ask
  liftEff do
    rpcConts <- readRef rpcContsRef
    writeRef rpcContsRef $ Map.insert rpcId {reply,complete} rpcConts



unregisterReplyComplete :: forall rep com m eff
                         . MonadEff (ref :: REF | eff) m => RPCID -> WebSocketClientRPCT rep com m Unit
unregisterReplyComplete rpcId = WebSocketClientRPCT do
  {rpcContsRef} <- ask
  liftEff do
    rpcConts <- readRef rpcContsRef
    writeRef rpcContsRef $ Map.delete rpcId rpcConts


runReply :: forall rep com m eff
          . MonadEff (ref :: REF | eff) m => RPCID -> rep -> WebSocketClientRPCT rep com m Unit
runReply rpcId rep = WebSocketClientRPCT do
  {rpcContsRef} <- ask
  rpcConts <- liftEff (readRef rpcContsRef)
  case Map.lookup rpcId rpcConts of
    Nothing -> pure unit -- TODO throw warning?
    Just {reply} -> lift (reply rep)


runComplete :: forall rep com m eff
             . MonadEff (ref :: REF | eff) m => RPCID -> com -> WebSocketClientRPCT rep com m Unit
runComplete rpcId com = WebSocketClientRPCT do
  {rpcContsRef} <- ask
  rpcConts <- liftEff (readRef rpcContsRef)
  case Map.lookup rpcId rpcConts of
    Nothing -> pure unit -- TODO throw warning?
    Just {complete} -> lift (complete com)
