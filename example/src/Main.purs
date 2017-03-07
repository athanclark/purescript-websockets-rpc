module Main where

import Prelude
import WebSocket (newWebSocket, URL (URL))
import WebSocket.RPC
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Generic (class Generic, gShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Timer (setTimeout)
import Control.Monad.Eff.Random (RANDOM, randomInt)


data MySubDSL = Foo

derive instance genericMySubDSL :: Generic MySubDSL

instance showMySubDSL :: Show MySubDSL where
  show = gShow

instance encodeJsonMySubDSL :: EncodeJson MySubDSL where
  encodeJson Foo = encodeJson ([] :: Array Unit)

instance decodeJsonMySubDSL :: DecodeJson MySubDSL where
  decodeJson json = do
    (x :: Array Unit) <- decodeJson json
    pure Foo

data MySupDSL = Bar

derive instance genericMySupDSL :: Generic MySupDSL

instance showMySupDSL :: Show MySupDSL where
  show = gShow

instance encodeJsonMySupDSL :: EncodeJson MySupDSL where
  encodeJson Bar = encodeJson ([] :: Array Unit)

instance decodeJsonMySupDSL :: DecodeJson MySupDSL where
  decodeJson json = do
    (x :: Array Unit) <- decodeJson json
    pure Bar

data MyRepDSL = Baz

derive instance genericMyRepDSL :: Generic MyRepDSL

instance showMyRepDSL :: Show MyRepDSL where
  show = gShow

instance encodeJsonMyRepDSL :: EncodeJson MyRepDSL where
  encodeJson Baz = encodeJson ([] :: Array Unit)

instance decodeJsonMyRepDSL :: DecodeJson MyRepDSL where
  decodeJson json = do
    (x :: Array Unit) <- decodeJson json
    pure Baz

data MyComDSL = Qux

derive instance genericMyComDSL :: Generic MyComDSL

instance showMyComDSL :: Show MyComDSL where
  show = gShow

instance encodeJsonMyComDSL :: EncodeJson MyComDSL where
  encodeJson Qux = encodeJson ([] :: Array Unit)

instance decodeJsonMyComDSL :: DecodeJson MyComDSL where
  decodeJson json = do
    (x :: Array Unit) <- decodeJson json
    pure Qux


myClient :: forall eff. ClientAppT (WebSocketClientRPCT MyRepDSL MyComDSL (Eff (AllEffs (random :: RANDOM | eff)))) Unit
myClient = rpcClient \dispatch -> do
  liftEff $ log "Subscribing Foo..."
  dispatch
    { subscription: Foo
    , onReply: \{supply,cancel} Baz -> do
        log $ show Baz
        void $ setTimeout 1000 $ do
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
  void $ setTimeout 1000 $ do
    conn <- newWebSocket (URL "ws://localhost:8080") []
    execWebSocketClientRPCT $ myClient conn
