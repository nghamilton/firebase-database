module Network.Google.Firebase.Util where

import Data.Time.Clock.POSIX
import Prelude hiding (log)
import Data.String.Conversions
import System.IO
import Data.ByteString.Char8 as BS

getTimestamp :: IO Int
getTimestamp = round `fmap` getPOSIXTime

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
