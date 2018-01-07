{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module FirebaseTypes where

import Data.Aeson
import Data.Aeson.Types
import qualified Database.Firebase as FB
import qualified Network.HTTP.Nano as Nano
import Data.Text
import System.Environment
import Control.Applicative
import Data.String.Conversions
import GHC.Generics
import Data.HashMap.Lazy
import Control.Applicative as Ap
import Data.Maybe
import Data.List.Split
import Data.List as L
import Control.Concurrent

import Debug.Trace

class FirebaseContext a where
  fbCtx :: Maybe a -> FB.Location

class (Show a, ToJSON a, FromJSON a, FirebaseContext a) =>
      FirebaseData a  where
  getId :: Maybe a -> Maybe FirebaseId
  setId :: a -> Text -> a
  genId :: a -> IO (Maybe FirebaseId)
  fbLoc :: Maybe a -> Maybe FB.Location
  getId _ = Nothing
  fbLoc a = (fbCtx a <>) . cs <$> getId a

type FirebaseId = Text

data (FirebaseData t, Show t) =>
     Event t
  = Event { action :: DataChangeType
          , id :: Text
          , item :: Maybe t}
  | INVALID {reason::Text}
  deriving (Show)

instance ToJSON (Event t) where
  toJSON _ = Null

instance FromJSON (Event t) where
  parseJSON _ = Ap.empty

instance (FirebaseData t, FirebaseContext t) => FirebaseContext (Event t) where
  fbCtx (Just (Event {item = itm})) = fbCtx itm
  fbCtx Nothing = "/"

-- we need this to derive full location from an event for event data when item=Nothing::Maybe t
instance FirebaseData t =>
         FirebaseData (Event t) where
  getId (Just (Event {id = i})) = Just i
  getId Nothing = Nothing
  setId a i =
    a
    { FirebaseTypes.id = i
    }
  genId _ = return Nothing

data FirebaseData t =>
     StreamEvent t = StreamEvent
  { streamEventType :: StreamEventType
  , streamEventData :: Either String (StreamEventData t)
  } deriving (Show, Eq)

data StreamEventType
  = PUT
  | PATCH
  | KEEP_ALIVE
  | CANCEL
  | AUTH_REVOKED
  deriving (Show, Eq)

data FirebaseData t =>
     StreamEventData t = StreamEventData
  { path :: FB.Location
  , changedData :: Maybe (HashMap FB.Location t)
  , changeAction :: DataChangeType
  } deriving (Show, Eq)

data DataChangeType
  = ADD
  | DELETE
  | UPDATE
  deriving (Show, Generic, Eq)

instance ToJSON DataChangeType

instance FromJSON DataChangeType

instance FirebaseData t =>
         FromJSON (StreamEventData t) where
  parseJSON (Object v) = pAddOne v <|> pAddAll v <|> pUpdateOrDelete v
  --parseJSON (Object v) = testp v
  parseJSON _ = Ap.empty

-- todo - create new method to accumulate error messages between alternative parser attempts so if the last parser fails it will return all error messages (i.e all Lefts concat'ed)
-- (<|>) :: Parser a -> Parser a -> Parser a
-- (<|>) p q = Parser $ \s ->
--   case parse p s of
--     []     -> parse q s
--     res    -> res

-- testp :: FirebaseData t => Object -> Parser (StreamEventData t)
-- testp v = do
--   eres :: StreamEventData t <- pAddOne v
--   case eres of
--     Right res -> return $ Right res
--     Left e -> do
--       eres2 :: StreamEventData t <- pAddAll v
--       case eres2 of
--         Right res2 -> return $ Right res2
--         Left e2 -> do
--           eres3 :: StreamEventData t <- pUpdateOrDelete v
--           case eres3 of
--             Right res3 -> return $ Right res3
--             Left e3 -> return $ Left "EventData parse failed all data types: "<>e<>", "<>e2<>","<>e3

pAddOne :: FirebaseData t => Object -> Parser (StreamEventData t)
--pAddOne v | trace (show v) False = undefined
pAddOne v = do
  p <- v .: "path"
  itm :: t <- v .: "data"
  -- if the parse was successful, it was a single new item added
  return $
      StreamEventData
      { path = p
      , changedData = mkData itm
      , changeAction = ADD
      } where
  mkData d = Just $ fromList $ L.map (\(i,d)->(cs $ fromJust i,d)) $ L.filter (\(i,_)->isJust i) [(getId (Just d), d)]

pAddAll :: FirebaseData t => Object -> Parser (StreamEventData t)
--pAddAll v | trace (show v) False = undefined
pAddAll v = do
  p <- v .: "path"
  ds :: HashMap FB.Location t <- v .: "data"
  -- if the parse was successful, it was multiple new data items added
  return $
         StreamEventData
         { path = p
         , changedData = Just $ ds
         , changeAction = ADD
         }

pUpdateOrDelete :: FirebaseData t => Object -> Parser (StreamEventData t)
--pUpdateOrDelete v | trace (show v) False = undefined
pUpdateOrDelete v = do
  p <- v .: "path"
  mt :: Maybe Text <- v .: "data"
  -- if the parse was successful, it means it is a path to an updated field
  if isJust mt
    then return $
      StreamEventData
      { path = p
      , changedData = Nothing
      , changeAction = UPDATE
      }
    -- otherwise it was a 'null' in the data
    else do
      if isFieldDelete p
         -- if the path is in the form of /x/y/ then it is a field delete in an item
         -- .... so we need to request an item update
        then return $
             StreamEventData
             { path = p
             , changedData = Nothing
             , changeAction = UPDATE
             }
        else return $
             StreamEventData
             { path = p
             , changedData = Nothing
             , changeAction = DELETE
             }
  where
    isFieldDelete p = L.length (wordsBy (== '/') p) >= 2

type FbId = Text

type FireMap t = HashMap FbId t

data FirebaseData t =>
        FireState t =
  FireState (MVar (FireMap t))
