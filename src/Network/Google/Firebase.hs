-- Copyright Ralph Morton (c) 2016

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

{-# LANGUAGE TupleSections #-}

module Network.Google.Firebase(
    module Network.Google.Firebase.Types,
    query,
    key,
    get,
    put,
    post,
    patch,
    delete,
    sendMessage,
    getRules,
    setRules
) where

import Network.Google.Firebase.Types hiding (PUT,DELETE)

import Control.Applicative ((<$>))
import Control.Lens (view)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Network.HTTP.Nano
import Network.HTTP.Types.URI (urlEncode)

query :: Query
query = Query Nothing Nothing Nothing Nothing

key :: ToJSON a => a -> Key
key = Key

get :: (FbHttpM m e r, HasFirebase r, FromJSON a) => Location -> Maybe Query -> m a
get loc mq = httpJSON =<< fbReq GET loc NoRequestData mq

put :: (FbHttpM m e r, HasFirebase r, ToJSON a) => Location -> a -> m ()
put loc dta = http' =<< fbReq PUT loc (mkJSONData dta) Nothing

post :: (FbHttpM m e r, HasFirebase r, ToJSON a) => Location -> a -> m FBID
post loc dta = unName <$> (httpJSON =<< fbReq POST loc (mkJSONData dta) Nothing)

patch :: (FbHttpM m e r, HasFirebase r, ToJSON a) => Location -> a -> m ()
patch loc dta = http' =<< fbReq (CustomMethod "PATCH") loc (mkJSONData dta) Nothing

delete :: (FbHttpM m e r, HasFirebase r) => Location -> m ()
delete loc = http' =<< fbReq DELETE loc NoRequestData Nothing

sendMessage :: (FbHttpM m e r, HasFirebase r, ToJSON a) => Message a -> m ()
sendMessage msg = do
    req <- buildReq POST googleCloudEndpoint (mkJSONData msg)
    apiKey <- view firebaseToken
    let req' = addHeaders [json, auth apiKey] req
    http' req'
    where
        json = ("Content-Type", "application/json")
        auth t = ("Authorization", "key=" ++ t)
        googleCloudEndpoint = "https://fcm.googleapis.com/fcm/send"

getRules :: (FbHttpM m e r, HasFirebase r, FromJSON a) => m a
getRules = httpJSON =<< fbReq GET ".settings/rules" NoRequestData Nothing

setRules :: (FbHttpM m e r, HasFirebase r, ToJSON a) => a -> m ()
setRules dta = http' =<< fbReq PUT ".settings/rules" (mkJSONData dta) Nothing

fbReq :: (FbHttpM m e r, HasFirebase r) => HttpMethod -> Location -> RequestData -> Maybe Query -> m Request
fbReq mthd loc dta mq = do
    baseURL <- view firebaseURL
    token <- view firebaseToken
    let qstr = maybe "" buildQuery mq
    let url = baseURL ++ loc ++ ".json?auth=" ++ token ++ "&" ++ qstr
    buildReq mthd url dta

buildQuery :: Query -> String
buildQuery q = intercalate "&" . fmap (uncurry toParam) $ catMaybes [orderByQ, startAtQ, endAtQ, limitQ $ limit q]
    where
    orderByQ = ("orderBy",) . show <$> orderBy q
    startAtQ = ("startAt",) . encodeK <$> startAt q
    endAtQ = ("endAt",) . encodeK <$> endAt q
    limitQ Nothing = Nothing
    limitQ (Just (LimitToFirst l)) = Just ("limitToFirst", show l)
    limitQ (Just (LimitToLast l)) = Just ("limitToLast", show l)

encodeK :: Key -> String
encodeK (Key a) = BL.unpack $ encode a

toParam :: String -> String -> String
toParam k v = k ++ "=" ++ (B.unpack . urlEncode False $ B.pack v)

---- end (c) Ralph Morton
