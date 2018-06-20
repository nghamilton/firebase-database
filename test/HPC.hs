import Data.List (genericLength)
import Data.Maybe (catMaybes)
import System.Exit (exitFailure, exitSuccess)

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / genericLength xs

expected :: Fractional a => a
expected = 90

main :: IO ()
main = undefined
