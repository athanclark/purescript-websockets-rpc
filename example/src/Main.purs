module Main where

import Prelude
import WebSocket (WEBSOCKET)
import WebSocket.RPC
import WebSocket.RPC.ACKable (ackableRPCClient)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Generic (class Generic, gShow)
import Data.UUID (GENUUID)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)


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


myClient :: forall eff
          . RPCClient MySubDSL MySupDSL MyRepDSL MyComDSL
               (Eff ( ws :: WEBSOCKET
                    , ref :: REF
                    , err :: EXCEPTION
                    , console :: CONSOLE
                    , random :: RANDOM
                    , timer :: TIMER
                    | eff))
myClient =
  { subscription: Foo
  , onSubscribe: \_ -> do
      log "subscribed..."
  , onReply: \{supply,cancel} Baz -> do
      log $ show Baz
      void $ setTimeout 1000 $ do
        log "supplying Bar..."
        supply Bar
  , onComplete: \Qux ->
      log $ show Qux
  }


main :: forall eff
      . Eff ( ws :: WEBSOCKET
            , ref :: REF
            , err :: EXCEPTION
            , console :: CONSOLE
            , random :: RANDOM
            , timer :: TIMER
            , uuid :: GENUUID
            | eff) Unit
main = do
  let runM = id
  log "test..."
  client <- ackableRPCClient runM "client" myClient
  void $ setTimeout 1000 $ execWebSocketClientRPCT $
    rpcClient runM (\dispatch -> do
        liftEff $ log "Subscribing Foo..."
        dispatch client)
      {url : "ws://localhost:8080", protocols : []}
