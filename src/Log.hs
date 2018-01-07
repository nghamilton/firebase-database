module Log where
import Data.Text as T
import Data.Text.IO as T

e :: Text -> IO ()
e = T.putStrLn
