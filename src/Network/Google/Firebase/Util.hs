module Network.Google.Firebase.Util where

import Data.Time.Clock.POSIX
import Web.Hashids as Hash
import Data.Text as T
import qualified Data.Text.IO as T
import Data.String.Conversions
import Control.Applicative
import Prelude hiding (log)
import Data.String.Conversions
import System.IO
import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy as BSL

getTimestamp :: IO Int
getTimestamp = round `fmap` getPOSIXTime

rightToMaybe :: Either b a -> Maybe a
rightToMaybe = either (const Nothing) Just

leftToMaybe  :: Either b a -> Maybe b
leftToMaybe = either Just (const Nothing)

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither b (Just a) = Right a
maybeToEither b Nothing = Left b

log :: BS.ByteString -> BS.ByteString -> IO ()
log level d = BS.hPutStrLn stdout (level<>": "<>d) >> (hFlush stdout)
logD d = log "DEBUG" d
logI d = log "INFO" d
logW d = log "WARN" d
logE d = log "ERROR" d
logWTF d = log "WTF" d
