{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Google.Firebase.Events where

import Network.Google.Firebase as FB
import Network.Google.Firebase.Types
import Network.Google.Firebase.Util
import Network.Google.Firebase.Settings

import Control.Lens.Lens
import System.IO.Streams.Attoparsec.ByteString
import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.ByteString as AB
import Data.ByteString.Char8 as BSC (putStrLn)
import Data.ByteString as S (ByteString, putStrLn)
import Data.Text as T
import System.IO.Streams as Streams
import Control.Monad as M
import GHC.Generics
import Data.Aeson
import Data.String.Conversions
import Control.Applicative as Ap
import Data.Maybe
import qualified Data.List as L hiding (lookup)
import Network.Http.Client hiding (PUT, PATCH, DELETE)
import Network.HTTP.Nano as Nano hiding (http, GET, PUT, PATCH)
import OpenSSL
import Control.Concurrent
import Control.Monad
import System.IO
import qualified Data.HashMap.Lazy as HM
import Data.List.Split
import Control.Monad.Except
import Control.Monad.Reader

-- connect to firebase server over https, and convert the event-stream to a Stream, saving it in the state
listen
  :: FirebaseData t
  => String -> String -> FireState t -> IO ()
listen tok loc' st =
  withOpenSSL $
  do ctx <- baselineContextSSL
     let loc = loc' ++ ".json?auth="
     logD $ cs $ "Listening on firebase location for updates: " <> loc
     withConnection (openConnectionSSL ctx (cs $ fbServer) 443) $
       (\c -> do
          let q =
                buildRequest1 $
                do http GET (cs $ loc ++ fbDataKey) >>
                     setAccept "text/event-stream"
          sendRequest c q emptyBody
          receiveResponse
            c
            (\p stream
                -- transform the stream into a stream of our desired FirebaseData type, and read an event
              -> do
               stream' <- transformStream stream
               -- fetch data for any UPDATE events in the stream and save to the state
               Streams.mapM (\t -> runF tok (fetchUpdates loc t)) stream'
               let loop = do
                     mEvnt :: Maybe (Event t) <- Streams.read stream'
                     case mEvnt of
                       Just (Event {action = act
                                   ,id = i
                                   ,item = itm})
                       -- update the database with the given event
                        -> do
                         updateDb st i itm
                         logRecordUpdate act itm
                         loop
                       Just (INVALID msg) ->
                         logW ("Invalid event encountered: " <> cs msg) >> loop
                       Nothing -> logW "Nothing encountered in stream. EOS."
               loop))
     logW "Firebase connection closed."

runF :: String -> FirebaseM t -> IO (Either Nano.HttpError t)
runF tok a = do
  env <- fbEnv tok
  runExceptT $ flip runReaderT env a

fbEnv :: String -> IO FBEnv
fbEnv tok = do
  let fb = FB.Firebase tok fbDataUrl
  mgr <- Nano.tlsManager
  let httpc = Nano.HttpCfg mgr
  return $ FBEnv fb httpc

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

updateDb
  :: FirebaseData t
  => FireState t -> FbId -> Maybe t -> IO ()
updateDb (FireState m) i Nothing = modifyMVar_ m (return . HM.delete i)
updateDb (FireState m) i (Just itm) = modifyMVar_ m (return . HM.insert i itm)

logRecordUpdate
  :: FirebaseData t
  => DataChangeType -> Maybe t -> IO ()
logRecordUpdate action itm = logD $ cs $ show itm ++ " (" ++ show action ++ ")"

-- run the parser over the bytestring stream from the server
-- nb: the Stream ByteSting returns 'chunks' that appear to correspond directly to actions
-- ... which makes the conversion from one stream to the other a little unnecessary, but safer?
transformStream
  :: FirebaseData t
  => InputStream ByteString -> IO (InputStream (Event t))
transformStream i =
  bytesToStreamEvents i >>= streamEventsToDataEvents >>= Streams.concatLists

-- logStreamData >>=
logStreamData :: InputStream ByteString -> IO ()
logStreamData i = Streams.peek i >>= (maybe (return ()) BSC.putStrLn)

-- converts byte stream into raw stream-event event types
bytesToStreamEvents
  :: FirebaseData t
  => InputStream ByteString -> IO (InputStream (StreamEvent t))
bytesToStreamEvents i = logStreamData i >> parserToInputStream parser i
  where
    parser = (AC.endOfInput >> pure Nothing) <|> pStreamEvent

-- converts raw stream-event event types into our data events (add/delete/update)
streamEventsToDataEvents
  :: FirebaseData t
  => InputStream (StreamEvent t) -> IO (InputStream [Event t])
streamEventsToDataEvents = Streams.map streamEventToDataEvent

-- need to get the latest data for any update event on an item
fetchUpdates
  :: (FbHttpM m e r, HasFirebase r, FirebaseData t)
  => String -> Event t -> m (Event t)
fetchUpdates loc e@(INVALID _) = return e
fetchUpdates loc evnt =
  case action evnt of
    UPDATE -> do
      itm <- FB.get loc Nothing
      return $
        evnt
        { item = Just itm
        }
    _ -> return evnt

streamEventToDataEvent
  :: FirebaseData t
  => StreamEvent t -> [Event t]
streamEventToDataEvent e =
  case streamEventType e of
    KEEP_ALIVE -> []
    CANCEL -> []
    AUTH_REVOKED -> []
    PUT -> convertFromStreamEvent' $ streamEventData e
    PATCH -> convertFromStreamEvent' $ streamEventData e
  where
    convertFromStreamEvent' (Right streamEventData) =
      convertFromStreamEvent
        (changeAction streamEventData)
        (changedData streamEventData) $
      path streamEventData
    convertFromStreamEvent' (Left err) =
      [ INVALID $
        "Event stream conversion of data, no event able to be produced: " <>
        (cs $ show err)
      ] -- Invalid condition, should only happen in PUT/PATCH msg if we have a stream corruption

-- stream events can contain a list of changed locations, and the action performed on them
convertFromStreamEvent
  :: FirebaseData t
  => DataChangeType
  -> Maybe (HM.HashMap FB.Location t)
  -> FB.Location
  -> [Event t]
convertFromStreamEvent act (Just ts) _ = L.map conv $ HM.toList ts
  where
    conv (loc, itm) =
      if validId -- for data security, getId of item must match the path being fetched
        then Event
             { action = act
             , id = locId
             , item = convertedItem
             }
        else INVALID $
             "Attempt to use invalid ID to update item:" <> (cs $ show loc) <>
             " (" <>
             (cs $ show act) <>
             ")"
      where
        mloc = idFromLoc loc
        validId = isJust mloc
        locId = fromJust mloc
        convertedItem =
          case act of
            ADD -> Just $ setId itm locId -- only ADD events have data available at this stage
            _ -> Nothing -- todo impl updates and other events
convertFromStreamEvent act Nothing loc -- No data supplied in the stream means it could be either UPDATE or DELETE
 =
  if validId
    then [ Event
           { action = act
           , id = locId
           , item = Nothing -- updated data needs to be retrieved manually from this location.. todo or can use generics here?
           }
         ]
    else [ INVALID $
           "Attempt to use invalid ID to update item:" <> (cs $ show loc) <>
           " (" <>
           (cs $ show act) <>
           ")"
         ]
  where
    mloc = idFromLoc loc
    validId = isJust mloc
    locId = fromJust mloc

idFromLoc :: FB.Location -> Maybe FbId
idFromLoc = (cs <$>) . headSafe . wordsBy (== '/')
  where
    headSafe [] = Nothing
    headSafe xs = Just $ L.head xs

-- Streams can have multiple stream events: put/patch/cancel/etc
pStreamEvent
  :: forall t.
     FirebaseData t
  => AB.Parser (Maybe (StreamEvent t))
pStreamEvent = do
  AC.string "event: "
  event <- pEventType
  AC.endOfLine
  AC.string "data: "
  edata <- pStreamEventData
  AC.endOfLine
  AC.endOfLine
  return $ Just $ StreamEvent event edata

pEventType :: AB.Parser StreamEventType
pEventType = do
  c <- AB.takeTill AC.isEndOfLine
  case c of
    "put" -> return PUT
    "patch" -> return PATCH
    "keep-alive" -> return KEEP_ALIVE
    "cancel" -> return CANCEL
    "auth_revoked" -> return AUTH_REVOKED
    _ -> mzero

-- The json decode will return a StreamEvent (with a Just t, or Left String if it was an error)
-- It will fail with an error if the 'data' element couldn't be parsed, ie if the data field was not a t or a null
-- ... this can happen if it was either a field name that was updated (not a whole data item) or a keep-alive
-- ... or if the app is writing data that doesn't conform to the backend (i.e missing fields, etc)
pStreamEventData
  :: forall t.
     (FromJSON t, FirebaseData t)
  => AB.Parser (Either String (StreamEventData t))
pStreamEventData = eitherDecodeStrict <$> AB.takeTill AC.isEndOfLine
