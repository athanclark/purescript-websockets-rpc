module WebSocket.RPC where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Eff.Var (($=), get)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, printJson, jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import WebSocket (Connection(Connection), runMessage, runMessageEvent, WEBSOCKET, Message(Message), ReadyState(Open))
import WebSocket.RPC.Trans.Client (WebSocketClientRPCT, freshRPCID, getClientEnv, registerReplyComplete, runComplete, runReply, runWebSocketClientRPCT', unregisterReplyComplete)
import WebSocket.RPC.Types (RPCIdentified(RPCIdentified), Subscribe(Subscribe), Supply(Supply), Reply(Reply), Complete(Complete), ServerToClient(..))



type RPCClientParams sup m =
  { supply :: sup -> m Unit
  , cancel :: m Unit
  }

type RPCClient sub sup rep com m =
  { subscription :: sub
  , onReply      :: RPCClientParams sup m -> rep -> m Unit
  , onComplete   :: com -> m Unit
  }


type ClientAppT m a = Connection -> m a


type AllEffs eff =
  ( ref     :: REF
  , timer   :: TIMER
  , err     :: EXCEPTION
  , ws      :: WEBSOCKET
  , console :: CONSOLE
  | eff)


rpcClient :: forall sub sup rep com eff
           . ( EncodeJson sub
             , EncodeJson sup
             , DecodeJson rep
             , DecodeJson com
             )
          => (( RPCClient sub sup rep com (Eff (AllEffs eff)) -> WebSocketClientRPCT rep com (Eff (AllEffs eff)) Unit)
                -> WebSocketClientRPCT rep com (Eff (AllEffs eff)) Unit)
          -> ClientAppT (WebSocketClientRPCT rep com (Eff (AllEffs eff))) Unit
rpcClient userGo (Connection socket) =
  let go :: RPCClient sub sup rep com (Eff (AllEffs eff)) -> WebSocketClientRPCT rep com (Eff (AllEffs eff)) Unit
      go params@{subscription,onReply,onComplete} = do
        env <- getClientEnv

        _ident <- freshRPCID

        readyState <- liftEff $ get socket.readyState

        case readyState of
          Open -> do
            liftEff $ socket.send $ Message $ printJson $ encodeJson $ Subscribe $ RPCIdentified {_ident, _params: subscription}

            let supply :: sup -> Eff (AllEffs eff) Unit
                supply sup =
                  socket.send $ Message $ printJson $ encodeJson $ Supply $ RPCIdentified {_ident, _params: Just sup}

                cancel :: Eff (AllEffs eff) Unit
                cancel = do
                  socket.send $ Message $ printJson $ encodeJson $ Supply $ RPCIdentified {_ident, _params: (Nothing :: Maybe Unit)}
                  runWebSocketClientRPCT' env (unregisterReplyComplete _ident)

            registerReplyComplete _ident (onReply {supply,cancel}) onComplete

            let runRep :: Reply rep -> WebSocketClientRPCT rep com (Eff (AllEffs eff)) Unit
                runRep (Reply (RPCIdentified {_ident: _ident', _params}))
                  | _ident' == _ident = runReply _ident _params
                  | otherwise         = pure unit -- FIXME throw warning?

                runCom :: Complete com -> WebSocketClientRPCT rep com (Eff (AllEffs eff)) Unit
                runCom (Complete (RPCIdentified {_ident: _ident', _params}))
                  | _ident' == _ident = do
                      runComplete _ident' _params
                      unregisterReplyComplete _ident'
                  | otherwise = pure unit

            liftEff $ socket.onmessage $= \event -> do
              let received :: String
                  received = runMessage (runMessageEvent event)

              case decodeJson =<< jsonParser received of
                Left err -> log $ "WebSocket parse error: " <> err
                Right x -> case x of
                  Rep rep -> runWebSocketClientRPCT' env (runRep rep)
                  Com com -> runWebSocketClientRPCT' env (runCom com)

          _ ->
            void $ liftEff $ setTimeout 1000 $
              runWebSocketClientRPCT' env $ go params

  in  userGo go
