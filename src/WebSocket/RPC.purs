module WebSocket.RPC
  ( RPCClientParams, RPCClient, rpcClient
  , module WebSocket.RPC.Trans.Client
  , module WebSocket.RPC.Types
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (warn, log, errorShow, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, printJson, jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import WebSocket (WEBSOCKET)
import WebSocket.Class (newWebSocket)
import WebSocket.RPC.Trans.Client (WebSocketClientRPCT, execWebSocketClientRPCT, freshRPCID, getClientEnv, registerReplyComplete, runComplete, runReply, runWebSocketClientRPCT', unregisterReplyComplete)
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


-- | Given a (forgetful) natural transformation from `m` to `Eff`, and usage of a `dispatch` function,
--   create a `ClientAppT`. Note - it is your job to make sure `liftEff` and `runM` form an isomorphism -
--   if there is dangling state, I suggest storing it in a Ref instead of value-passing, so the read-only
--   monad environment is decoupled from the value of the monad.
rpcClient :: forall sub sup rep com eff m
           . ( EncodeJson sub
             , EncodeJson sup
             , DecodeJson rep
             , DecodeJson com
             , MonadEff (ref :: REF, err :: EXCEPTION, ws :: WEBSOCKET, console :: CONSOLE | eff) m
             )
          => (forall a. m a -> Eff (ref :: REF, err :: EXCEPTION, ws :: WEBSOCKET, console :: CONSOLE | eff) a)
          -> ( (RPCClient sub sup rep com m -> WebSocketClientRPCT rep com m Unit)
             -> WebSocketClientRPCT rep com m Unit)
          -> {url :: String, protocols :: Array String}
          -> (WebSocketClientRPCT rep com m) Unit
rpcClient runM userGo {url,protocols} =
  let go :: RPCClient sub sup rep com m -> WebSocketClientRPCT rep com m Unit
      go params@{subscription,onReply,onComplete} = do
        env <- getClientEnv

        _ident <- freshRPCID

        newWebSocket (runM <<< runWebSocketClientRPCT' env)
          { url
          , protocols
          , continue: \_ ->
              { onclose: \{code,reason,wasClean} -> liftEff $ do
                  log $ "-- WebSocketRPC closed ---"
                  log $ "code:      " <> show code
                  log $ "reason:    " <> show reason
                  log $ "was clean: " <> show wasClean
                  log   "--------------------------"
              , onerror: \e -> liftEff $ errorShow e
              , onopen: \{send} -> do
                  send $ printJson $ encodeJson $ Subscribe $ RPCIdentified {_ident, _params: subscription}

                  let supply :: sup -> m Unit
                      supply sup = runWebSocketClientRPCT' env $
                        send $ printJson $ encodeJson $ Supply $ RPCIdentified {_ident, _params: Just sup}

                      cancel :: m Unit
                      cancel = runWebSocketClientRPCT' env $ do
                        send $ printJson $ encodeJson $ Supply $ RPCIdentified {_ident, _params: (Nothing :: Maybe Unit)}
                        unregisterReplyComplete _ident

                  registerReplyComplete _ident (onReply {supply,cancel}) onComplete
              , onmessage: \{send} received -> do
                  let runRep :: Reply rep -> WebSocketClientRPCT rep com m Unit
                      runRep (Reply (RPCIdentified {_ident: _ident', _params}))
                        | _ident' == _ident = runReply _ident _params
                        | otherwise         = pure unit -- FIXME throw warning?

                      runCom :: Complete com -> WebSocketClientRPCT rep com m Unit
                      runCom (Complete (RPCIdentified {_ident: _ident', _params}))
                        | _ident' == _ident = do
                            runComplete _ident' _params
                            -- unregisterReplyComplete _ident'
                        | otherwise = pure unit

                  case decodeJson =<< jsonParser received of
                    Left err -> liftEff $ warn $ "WebSocket parse error: " <> err
                    Right x -> case x of
                      Rep rep -> runRep rep
                      Com com -> runCom com
                      Ping    -> send $ printJson $ encodeJson ([] :: Array Unit)
              }
          }

  in  userGo go
