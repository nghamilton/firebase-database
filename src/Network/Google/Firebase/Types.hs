{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

module Network.Google.Firebase.Types where

import Data.Aeson.Types
import Network.HTTP.Nano hiding (DELETE)
import Data.Text
import Data.String.Conversions
import GHC.Generics hiding (to)
import Data.HashMap.Lazy as HashMap
import Control.Applicative as Ap
import Data.Maybe
import Data.List.Split
import qualified Data.List as L
import Control.Concurrent
import Control.Applicative ((<$>))
import Control.Lens.TH
import Control.Monad (mzero)
import Control.Monad.Reader  (MonadReader)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans (MonadIO)
import qualified Data.Text.Internal as T
import Data.Proxy
import GHC.TypeLits

type family FirebaseContext a :: Symbol

class (Show a, ToJSON a, FromJSON a, KnownSymbol (FirebaseContext a)) =>
      FirebaseData a  where
  getId :: a -> Maybe FirebaseId
  setId :: a -> Text -> a
  genId :: a -> IO (Maybe FirebaseId)
  fbLoc :: a -> Maybe Location
  getId _ = Nothing
  fbLoc a = (symbolVal (Proxy::Proxy (FirebaseContext a)) <>) . cs <$> getId a

type FirebaseId = Text

data (FirebaseData t, Show t) =>
     Event t
  = Event { action :: DataChangeType
          , id :: Text
          , item :: Maybe t}
  | INVALID {reason::Text}
  deriving (Show)

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
  { path :: Location
  , changedData :: Maybe (HashMap Location t)
  , changeAction :: DataChangeType
  } deriving (Show, Eq)

data DataChangeType
  = ADD
  | DELETE
  | UPDATE
  deriving (Show, Generic, Eq)

type FbId = Text

type FireMap t = HashMap FbId t

data FirebaseData t =>
        FireState t =
  FireState (MVar (FireMap t))

instance ToJSON (Event t) where
  toJSON _ = Null

instance FromJSON (Event t) where
  parseJSON _ = Ap.empty

instance ToJSON DataChangeType

instance FromJSON DataChangeType

instance FirebaseData t =>
         FromJSON (StreamEventData t) where
  parseJSON (Object v) = pAddOne v <|> pAddAll v <|> pUpdateOrDelete v
  --parseJSON (Object v) = testp v
  parseJSON _ = Ap.empty

pAddOne :: FirebaseData t => Object -> Parser (StreamEventData t)
--pAddOne v | trace (show v) False = undefined
pAddOne v = do
  p <- v .: "path"
  itm :: t <- v .: "data"
  -- if the parse was successful, it was a single new item added
  return
      StreamEventData
      { path = p
      , changedData = mkData itm
      , changeAction = ADD
      } where
  mkData d = Just $ fromList $ L.map (\(i,d')->(cs $ fromJust i,d')) $ L.filter (\(i,_)->isJust i) [(getId d, d)]

pAddAll :: FirebaseData t => Object -> Parser (StreamEventData t)
--pAddAll v | trace (show v) False = undefined
pAddAll v = do
  p <- v .: "path"
  ds :: HashMap Location t <- v .: "data"
  -- if the parse was successful, it was multiple new data items added
  return
         StreamEventData
         { path = p
         , changedData = Just ds
         , changeAction = ADD
         }

pUpdateOrDelete :: FirebaseData t => Object -> Parser (StreamEventData t)
--pUpdateOrDelete v | trace (show v) False = undefined
pUpdateOrDelete v = do
  p <- v .: "path"
  mt :: Maybe Text <- v .: "data"
  -- if the parse was successful, it means it is a path to an updated field
  if isJust mt
    then return
      StreamEventData
      { path = p
      , changedData = Nothing
      , changeAction = UPDATE
      }
    -- otherwise it was a 'null' in the data
    else
      if isFieldDelete p
         -- if the path is in the form of /x/y/ then it is a field delete in an item
         -- .... so we need to request an item update
        then return
             StreamEventData
             { path = p
             , changedData = Nothing
             , changeAction = UPDATE
             }
        else return
             StreamEventData
             { path = p
             , changedData = Nothing
             , changeAction = DELETE
             }
  where
    isFieldDelete p = L.length (wordsBy (== '/') p) >= 2


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


--- Code below copyright: Copyright Ralph Morton (c) 2016

-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:

--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.

--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.

--     * Neither the name of Ralph Morton nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

type Location = String
type FBID = String
type FbHttpM m e r = (MonadIO m, MonadError e m, MonadReader r m, AsHttpError e, HasHttpCfg r)

data Firebase = Firebase {
    _firebaseToken :: String,
    _firebaseURL :: String
}

-- |Representation of a Firebase name response to a POST
newtype Name = Name { unName :: String } deriving Show

instance FromJSON Name where
    parseJSON (Object v) = Name <$> v .: "name"
    parseJSON _ = mzero

data Query = Query {
    orderBy :: Maybe Location,
    startAt :: Maybe Key,
    endAt :: Maybe Key,
    limit :: Maybe Limit
}

data Message a = Message {
    to                  :: Maybe String,
    registrationIDs     :: [String],
    collapseKey         :: Maybe String,
    priority            :: Priority,
    contentAvailable    :: Maybe Bool,
    delayWhileIdle      :: Maybe Bool,
    ttl                 :: Maybe Int,
    payload             :: MessageBody a
}

instance ToJSON a => ToJSON (Message a) where
    toJSON Message {..} =
        omitNulls [ "to" .= toJSON to,
                    "registration_ids" .= toJSON registrationIDs,
                    "collapse_key" .= toJSON collapseKey,
                    "priority" .= toJSON priority,
                    "content_available" .= toJSON contentAvailable,
                    "delay_while_idle" .= toJSON delayWhileIdle,
                    "time_to_live" .= toJSON ttl,
                    case payload of
                         Notification x -> "notification" .= toJSON x
                         Data x -> "data" .= toJSON x
        ]

data MessageBody a = Notification a | Data a

data Priority = Low | High

instance ToJSON Priority where
    toJSON Low = String "low"
    toJSON High = String "high"

data Limit = LimitToFirst Int | LimitToLast Int

data Key = forall a. ToJSON a => Key a

omitNulls :: [(T.Text, Value)] -> Value
omitNulls = object . L.filter notNull where
    notNull (_, Null) = False
    notNull _         = True

makeClassy ''Firebase

