{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Firebase where

import FirebaseTypes
import FirebaseAuth
import Types
import TestData as TD
import Log as Log
import Util
import Settings

import Data.HashMap.Lazy as HM
import Control.Lens.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import qualified Database.Firebase as FB
import qualified Network.HTTP.Nano as Nano
import Data.Text
import System.Environment
import Data.Monoid
import TextShow
import Data.String.Conversions
import Data.List as L
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S
import Network.Http.Client
import OpenSSL
import Data.Time.LocalTime
import Control.Arrow

data FBEnv = FBEnv
  { fbInstance :: FB.Firebase
  , fbHttpCfg :: Nano.HttpCfg
  }

instance Nano.HasHttpCfg FBEnv where
  httpCfg =
    lens
      fbHttpCfg
      (\te h ->
          te
          { fbHttpCfg = h
          })

instance FB.HasFirebase FBEnv where
  firebase =
    lens
      fbInstance
      (\te f ->
          te
          { fbInstance = f
          })

--todo supply this to each method call, or create monad for us
type FirebaseM = ReaderT FBEnv (ExceptT Nano.HttpError IO)

fetch
  :: FirebaseData d
  => Maybe FB.Location -> IO (Either InternalError d)
fetch Nothing = return $ Left $ InternalError $ InvalidDataLocation
fetch (Just loc) = (fixError <$>) $ runFData $ FB.get loc Nothing

persist
  :: (ToJSON d, FirebaseData d, Show d)
  => d -> IO (Either InternalError FirebaseId)
persist = (fixError <$>) . runFData . persist'

fixError :: Show e => Either e b -> Either InternalError b
fixError = left (\e->InternalError $ DatastoreError $ "Data update failed: "<>(cs $ show e))

-- perform a direct save of the data to a given location
-- ... probably should build types and do a read-update instead?
rawUpdate :: Text -> Text -> IO (Maybe InternalError)
rawUpdate loc val = (leftToMaybe.fixError <$>) $ runFData $ FB.patch (cs loc) $ object [val .= True]

sendMessage
  :: ToJSON msg
  => FcmToken -> msg -> IO (Either InternalError ())
sendMessage to d = (fixError <$>) $ runFMsg $ FB.sendMessage msg
  where
    msg =
      FB.Message
        { to = Nothing --Just (cs to)
      , FB.registrationIDs = [cs to]
      , FB.collapseKey = Nothing
      , FB.priority = FB.High
      , FB.contentAvailable = Nothing
      , FB.delayWhileIdle = Nothing
      , FB.ttl = Nothing
      , FB.payload = FB.Notification d
      }

-- todo find a better spot for these?
addMerchantTransaction :: Mid -> Tid -> IO (Maybe InternalError)
addMerchantTransaction mid tid =
  rawUpdate ("/accounts/" <> mid <> "/merchant/transactions/") tid

addClientTransaction :: Cid -> Tid -> IO (Maybe InternalError)
addClientTransaction cid tid =
  rawUpdate ("/accounts/" <> cid <> "/client/transactions/") tid

fbEnv :: String -> IO FBEnv
fbEnv tok = do
  let fb = FB.Firebase tok fbDataUrl
  mgr <- Nano.tlsManager
  let httpc = Nano.HttpCfg mgr
  return $ FBEnv fb httpc

runFData = runF fbDataKey
runFMsg = runF fbMessagingKey

runF :: String -> FirebaseM t -> IO (Either Nano.HttpError t)
runF tok a = do
  env <- fbEnv tok
  runExceptT $ flip runReaderT env a

persist'
  :: (ToJSON d, FirebaseData d, Show d)
  => d -> FirebaseM FirebaseId
persist' d = do
  lastMod <- show <$> liftIO getZonedTime
  -- inject lastModified timestamp
  let fbData =
        case toJSON d of
          Object j -> Object $ HM.insert "lastModified" (toJSON lastMod) j
          others -> others
  mi <-
    do case getId (Just d) of
         Nothing -> liftIO $ genId d
         Just i -> return $ Just i
  case mi of
    Nothing -> cs <$> FB.post (fbCtx (Just d)) fbData
    Just i -> FB.put (fbCtx (Just d) <> (cs $ i)) fbData >> return i
-- testMain tok url = do
--     let fb = FB.Firebase tok url
--     mgr <- Nano.tlsManager
--     let httpc = Nano.HttpCfg mgr
--     let te = FBEnv fb httpc
--     res <- runExceptT $ flip runReaderT te test
--     case res of
--         Left e -> error $ show e
--         Right r -> return ()
-- test :: FirebaseM ()
-- test = do
--     put "/test/a" $ object ["val" .= ("test value" :: String)]
--     ra <- (get "/test/a" Nothing :: FirebaseM Value)
--     liftIO $ putStrLn (show ra)
--     mapM_ (post "b") ([1..10] :: [Int])
--     rb <- (get "b" Nothing :: FirebaseM Value)
--     liftIO $ putStrLn (show rb)
--     put "c" $ object ["a" .= ("va" :: String), "b" .= ("vb" :: String)]
--     patch "c" $ object ["a" .= ("va_patched" :: String)]
--     rx <- (get "" (Just $ FB.query { FB.orderBy = Just "$key", FB.startAt = Just $ FB.key ("c" :: String) }) :: FirebaseM Value)
--     liftIO $ putStrLn (show rx)
--
