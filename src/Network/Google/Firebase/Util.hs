{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

module Network.Google.Firebase.Util where

import Network.Google.Firebase.Types
import Network.Google.Firebase

import Prelude hiding (log)
import Data.String.Conversions
import System.IO
import Data.ByteString.Char8 as BS
import Control.Monad.Except
import Control.Monad.Reader
import Network.HTTP.Nano as Nano hiding (http, GET, PUT, PATCH)
import Control.Lens.Lens
import Data.Proxy
import GHC.TypeLits
import Data.HashMap.Lazy as HM
import Data.Aeson
import Control.Arrow

rightToMaybe :: Either b a -> Maybe a
rightToMaybe = either (const Nothing) Just

leftToMaybe :: Either b a -> Maybe b
leftToMaybe = either Just (const Nothing)

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just a) = Right a
maybeToEither b Nothing = Left b

log :: BS.ByteString -> BS.ByteString -> IO ()
log level d = BS.hPutStrLn stdout (level <> ": " <> d) >> hFlush stdout

logD :: ByteString -> IO ()
logD = log "DEBUG"

logI :: ByteString -> IO ()
logI = log "INFO"

logW :: ByteString -> IO ()
logW = log "WARN"

logE :: ByteString -> IO ()
logE = log "ERROR"

logWTF :: ByteString -> IO ()
logWTF = log "WTF"

fbCtxFromState :: forall a. KnownSymbol (FirebaseContext a) => FireState a -> String
fbCtxFromState _ = symbolVal (Proxy :: Proxy (FirebaseContext a))

fbEnv :: String -> String -> IO FBEnv
fbEnv url tok = do
  let fb = Firebase tok url
  mgr <- Nano.tlsManager
  let httpc = Nano.HttpCfg mgr
  return $ FBEnv fb httpc

fetch
  :: FirebaseData d
  => String -> String -> Maybe Location -> IO (Either FirebaseError d)
fetch _ _ Nothing = return $ Left InvalidLocation
fetch url tok (Just loc) = (fixError <$>) $ (runF url tok) $ get loc Nothing

persist
  :: (ToJSON d, FirebaseData d, Show d)
  => String -> String -> d -> IO (Either FirebaseError FirebaseId)
persist url tok = (fixError <$>) . (runF url tok). persist'

fixError :: Show e => Either e b -> Either FirebaseError  b
fixError = left (\e->CommsError $ "Data update failed: "<>(cs $ show e))

runF :: String -> String -> FirebaseM t -> IO (Either Nano.HttpError t)
runF url tok a = do
  env <- fbEnv url tok
  runExceptT $ flip runReaderT env a

-- fix this mess
persist'
  :: (ToJSON d, FirebaseData d, Show d)
  => d -> FirebaseM FirebaseId
persist' d = do
  let lastMod = "6/6/6"::String
  let fbData =
        case toJSON d of
          Object j -> Object $ HM.insert "lastModified" (toJSON lastMod) j
          others -> others
  case fbLoc d of
    Just loc -> do
      put loc fbData
      case getId d of
        Just i -> return i
        Nothing -> undefined --arrgg
    Nothing -> do
      mnewId <- liftIO $ genId d
      case mnewId of
        Just newId -> do
          let loc = fbCtx d <> cs newId
          put loc fbData
          return newId
        Nothing -> cs <$> post (fbCtx d) fbData -- if no ID then we get FB to make us one

data FBEnv = FBEnv
  { fbInstance :: Firebase
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

instance HasFirebase FBEnv where
  firebase =
    lens
      fbInstance
      (\te f ->
          te
          { fbInstance = f
          })

--todo supply this to each method call, or create monad for us
type FirebaseM = ReaderT FBEnv (ExceptT Nano.HttpError IO)

