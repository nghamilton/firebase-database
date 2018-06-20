{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module FirebaseEventsSpec where

import Network.Google.Firebase
import Network.Google.Firebase.Types
import Network.Google.Firebase.Events

import TestData
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Test.QuickCheck.Monadic

-- `main` is here so that this module can be run from GHCi on its own.
-- It is not needed for automatic spec discovery.
main :: IO ()
main = hspec $ spec

-- Test parsing of firebase event-stream
spec :: Spec
spec = undefined
