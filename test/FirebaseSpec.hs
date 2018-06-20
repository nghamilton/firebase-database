{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module FirebaseSpec where

import Network.Google.Firebase
import Network.Google.Firebase.Util

import TestData
import Test.Hspec


-- `main` is here so that this module can be run from GHCi on its own.
-- It is not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

-- Test firebase functions
spec :: Spec
spec = do
  describe "Main" $
    do it "Saves an item to firebase." $
          do persist testFbUrl testFbTok persistTestItem >>= (`shouldBe` ((Right $ itmId persistTestItem )::Either FirebaseError FirebaseId))
       it "Retrieves an item from firebase." $
           do let loc = fbLoc persistTestItem
              fetch testFbUrl testFbTok loc >>= (`shouldBe` (Right $ persistTestItem))
--       it "Sends a message to test fcm." $
--           do sendMessage testFcmToken (object ["title" .= ("test msg" :: String)]) >>= (`shouldBe` (Right ()))
