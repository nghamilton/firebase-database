{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module TestData where

import Network.Google.Firebase

import Data.Text
import Data.Aeson
import GHC.Generics

persistTestItem :: TestType
persistTestItem = TestType {itmId="1",itmData="testdata"}

type instance FirebaseContext TestType = "/test/"

instance FirebaseData TestType where
  getId a = Just $ itmId (a :: TestType)
  setId a i =
    a
    { itmId = i
    }
  genId _ = return $ Just "1"

instance ToJSON TestType
instance FromJSON TestType
data TestType = TestType {itmId::Text,itmData::String} deriving (Show,Generic,Eq)

testFcmToken :: String
testFcmToken = ""
testFbTok :: String
testFbTok = ""
testFbUrl :: String
testFbUrl = ""
