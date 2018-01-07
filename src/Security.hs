module Security where

import Types

import Data.Digest.Pure.SHA (hmacSha256, showDigest)
import Data.Text as T
import Data.ByteString.Lazy as BSL
import Data.String.Conversions

makeHmac :: ClientHmacKey  -> BSL.ByteString -> Hmac
makeHmac key d = cs $ showDigest $ hmacSha256 (cs key) d

makeDigest :: ClientHmacKey  -> BSL.ByteString -> Text
makeDigest = (T.take 7 .) . makeHmac
