module WebSocket.RPC.ACKable where


import Prelude
import WebSocket.RPC
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Tuple (Tuple (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (.?), (:=), jsonEmptyObject, (~>))
import Data.UUID (UUID, parseUUID, GENUUID, genUUID)
import Data.Map as Map
import Data.Set as Set
import Data.Foldable (sum)
import Data.Int (pow)
import Data.Array ((..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef, modifyRef)
import Control.Monad.Eff.Console (CONSOLE, log, warnShow)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Aff (launchAff, runAff, later', Canceler)
import Control.Monad.Aff as Aff


newtype ACKable owner a = ACKable
  { ackableID    :: UUID
  , ackableOwner :: owner
  , ackableData  :: Maybe a
  }

instance encodeJsonACKable :: (EncodeJson owner, EncodeJson a) => EncodeJson (ACKable owner a) where
  encodeJson (ACKable {ackableID,ackableOwner,ackableData})
    =  "id" := show ackableID
    ~> "owner" := ackableOwner
    ~> "data" := ackableData
    ~> jsonEmptyObject

instance decodeJsonACKable :: (DecodeJson owner, DecodeJson a) => DecodeJson (ACKable owner a) where
  decodeJson json = do
    o <- decodeJson json
    id' <- o .? "id"
    case parseUUID id' of
      Nothing -> Left "Couldn't parse UUID"
      Just ackableID -> do
        ackableOwner <- o .? "owner"
        ackableData <- o .? "data"
        pure $ ACKable
          { ackableID
          , ackableOwner
          , ackableData
          }


ack :: forall owner a. UUID -> owner -> ACKable owner a
ack ackableID ackableOwner = ACKable
  { ackableID
  , ackableOwner
  , ackableData: Nothing
  }


ackableRPCClient :: forall sub sup rep com m owner eff
                  . ( MonadEff (ref :: REF, uuid :: GENUUID, console :: CONSOLE, err :: EXCEPTION | eff) m
                    , Ord owner
                    )
                 => (forall a. m a -> Eff (ref :: REF, uuid :: GENUUID, console :: CONSOLE, err :: EXCEPTION | eff) a)
                 -> owner
                 -> RPCClient sub sup rep com m
                 -> m (RPCClient (ACKable owner sub) (ACKable owner sup) (ACKable owner rep) com m)
ackableRPCClient runM clientOwner {subscription,onSubscribe,onReply,onComplete} = do
  supplyMailbox <- liftEff $ newRef Map.empty
  ownerPending <- liftEff $ newRef (Map.empty :: Map.Map owner (Set.Set UUID))

  let ackParams :: Maybe owner -> RPCClientParams (ACKable owner sup) m -> RPCClientParams sup m
      ackParams mOwner {supply,cancel} =
        { supply: \s -> do
             ackableID <- liftEff genUUID
             let op = do
                   liftEff $ log $ "Supplying: " <> show ackableID
                   supply $ ACKable
                     { ackableID
                     , ackableOwner: clientOwner
                     , ackableData: Just s
                     }
             liftEff $ do
               let onQuit = do
                      supplies <- readRef supplyMailbox
                      modifyRef supplyMailbox $ Map.delete ackableID
                      case mOwner of
                        Nothing -> pure unit
                        Just serverOwner -> modifyRef ownerPending $ Map.delete serverOwner
                      case Map.lookup ackableID supplies of
                        Nothing -> pure unit
                        Just (Tuple _ expBackoff) -> void $ runAff warnShow (\_ -> pure unit) $ Aff.cancel expBackoff $ error $ "closing exponential backoff mailbox for: " <> show ackableID
               expBackoff <- mkBackoff (runM op) onQuit
               modifyRef supplyMailbox $ Map.insert ackableID (Tuple s expBackoff)
               case mOwner of
                 Nothing -> pure unit
                 Just serverOwner -> modifyRef ownerPending $ Map.unionWith Set.union (Map.singleton serverOwner (Set.singleton ackableID))
             op
        , cancel
        }

  ackableID <- liftEff genUUID
  pure
    { subscription: ACKable
        { ackableID
        , ackableOwner: clientOwner
        , ackableData: Just subscription
        }
    , onSubscribe: onSubscribe <<< ackParams Nothing
    , onReply: \params (ACKable {ackableID,ackableOwner,ackableData}) -> case ackableData of
        Nothing -> liftEff $ do
          mExpBackoff <- do
            supplies <- readRef supplyMailbox
            owners <- readRef ownerPending
            case Map.lookup ackableID supplies of
              Nothing -> pure Nothing
              Just (Tuple _ expBackoff) -> do
                writeRef supplyMailbox $ Map.delete ackableID supplies
                writeRef ownerPending $ Map.update (Just <<< Set.delete ackableID) ackableOwner owners
                pure (Just expBackoff)
          case mExpBackoff of
            Nothing -> log $ "Somehow received an ACK that doesn't exist: " <> show ackableID
            Just expBackoff -> void $ runAff warnShow (\_ -> pure unit) $ Aff.cancel expBackoff $ error $ "acknowledged: " <> show ackableID

        Just rep -> do
          liftEff $ log $ "Acknowledging: " <> show ackableID
          params.supply (ack ackableID clientOwner)
          onReply (ackParams (Just clientOwner) params) rep
    , onComplete
    }



mkBackoff :: forall eff
           . Eff (ref :: REF, console :: CONSOLE | eff) Unit
          -> Eff (ref :: REF, console :: CONSOLE | eff) Unit
          -> Eff (ref :: REF, console :: CONSOLE | eff) (Canceler (ref :: REF, console :: CONSOLE | eff))
mkBackoff op x = do
  spentWaiting <- newRef 0
  runAff warnShow (\_ -> pure unit) $ do
    toWait <- liftEff $ readRef spentWaiting
    liftEff $ writeRef spentWaiting (toWait + 1)
    let toWait' = pow 2 toWait
        soFar = sum $ (\x -> (pow 2 x) * second) <$> (0 .. toWait)

    when (soFar > week) (liftEff x)

    later' (second * (toWait' + 10)) $ liftEff op


second = 1000
minute = 60 * second
hour = 60 * minute
day = 24 * hour
week = 7 * day
