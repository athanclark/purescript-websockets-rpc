module Main where

import Prelude
import WebSocket (newWebSocket)
import WebSocket.RPC
import Data.Argonaut (EncodeJson, DecodeJson, encodeJson, decodeJson)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Timer (setTimeout)
import Control.Monad.Eff.Random (RANDOM, randomInt)


data MySubDSL = Foo

instance encodeJsonMySubDSL :: EncodeJson MySubDSL where
  encodeJson Foo = encodeJson ([] :: Array Unit)

instance decodeJsonMySubDSL :: DecodeJson MySubDSL where
  decodeJson json = do
    (x :: Array Unit) <- decodeJson json
    pure Foo

data MySupDSL = Bar

instance encodeJsonMySupDSL :: EncodeJson MySupDSL where
  encodeJson Bar = encodeJson ([] :: Array Unit)

instance decodeJsonMySupDSL :: DecodeJson MySupDSL where
  decodeJson json = do
    (x :: Array Unit) <- decodeJson json
    pure Bar

data MyRepDSL = Baz

instance encodeJsonMyRepDSL :: EncodeJson MyRepDSL where
  encodeJson Baz = encodeJson ([] :: Array Unit)

instance decodeJsonMyRepDSL :: DecodeJson MyRepDSL where
  decodeJson json = do
    (x :: Array Unit) <- decodeJson json
    pure Baz

data MyComDSL = Qux

instance encodeJsonMyComDSL :: EncodeJson MyComDSL where
  encodeJson Qux = encodeJson ([] :: Array Unit)

instance decodeJsonMyComDSL :: DecodeJson MyComDSL where
  decodeJson json = do
    (x :: Array Unit) <- decodeJson json
    pure Qux


myClient :: forall eff. ClientAppT (WebSocketClientRPCT (Eff (AllEffs (random :: RANDOM | eff)))) Unit
myClient = rpcClient \dispatch -> do
  log "Subscribing Foo..."
  dispatch
    { subscription: Foo
    , onReply: \{supply,cancel} Baz -> do
        log $ show Baz
        setTimeout 1000 $ do
          log "supplying Bar..."
          supply Bar
          q <- randomInt 0 9
          when (q == 0) $ do
            log "Canceling..."
            cancel
    , onComplete: \Qux ->
        log $ show Qux
    }


main :: forall eff. Eff (AllEffs (random :: RANDOM | eff)) Unit
main =
  setTimeout 1000 $ do
    conn <- newWebSocket (URL "ws://localhost:8080") []
    execWebSocketClientRPCT $ myClient conn
