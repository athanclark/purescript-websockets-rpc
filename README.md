purescript-websockets-rpc
=======================


for use with a [websockets-rpc](https://hackage.haskell.org/package/websockets-rpc)-compliant websocket server,
the [purescript-websocket-simple](https://pursuit.purescript.org/packages/purescript-websocket-simple) client
library, and [purescript-argonaut](https://github.com/purescript-contrib/purescript-argonaut) json serialization
system.


Example
-------


```purescript
import WebSocket.RPC

data MySubDSL = Foo
  deriving (EncodeJson, DecodeJson) -- you should figure this out

data MySupDSL = Bar
  deriving (EncodeJson, DecodeJson)

data MyRepDSL = Baz
  deriving (EncodeJson, DecodeJson)

data MyComDSL = Qux
  deriving (EncodeJson, DecodeJson)





myClient :: forall eff. ClientAppT (WebSocketClientRPCT (Eff (AllEffs eff))) Unit
myClient = rpcClient $ \dispatch -> do
  -- could dispatch more than one subscription here
  dispatch myClient'
  where
    myClient' :: RPCClient MySubDSL MySupDSL MyRepDSL MyComDSL (Eff (AllEffs eff))
    myClient' =
      { subscription: Foo
      , onReply: \{supply,cancel} Baz -> do
          x <- randomInt 1 10
          if x == 10 then cancel else supply Bar
      , onComplete: \Qux ->
          log "ayooo"
      }
      
      
main :: Eff _ Unit
main = do
  conn <- newWebSocket (URL "localhost") []
  execWebSocketClientRPCT (myClient conn)
```
