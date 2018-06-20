{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module EventsSpec where
import Test.Hspec

import Data.Aeson
import Data.Text
import GHC.Generics
import Data.Monoid
import qualified Data.ByteString.Lazy as LBS

type Location = String
type FirebaseId = String

spec :: Spec
spec = undefined

---- library code
class FirebaseContext a where
  fbCtx :: a -> Location

class (Show a, ToJSON a, FromJSON a, FirebaseContext a) =>
      FirebaseData a  where
  getId :: a -> Maybe FirebaseId
  setId :: a -> Text -> a
  genId :: a -> IO (Maybe FirebaseId)
  fbLoc :: a -> Maybe Location
  getId _ = Nothing
  fbLoc a = (fbCtx a <>) <$> getId a

read :: (FromJSON d, FirebaseData d, Show d) => Maybe d -> IO (Either String d)
read mt = eitherDecode <$> LBS.readFile undefined


----------- client implementation of library code (save utilizes other library methods not demonstrated here)
save :: (ToJSON d, FirebaseData d, Show d) => d -> IO ()
save t = writeFile (fbCtx t) (show t)


---- client types
data MyType1 = MyType1 deriving (Show, Generic)
data MyType2 = MyType2 deriving (Show, Generic)

instance FirebaseContext MyType1 where
  fbCtx _ = "/MyType1Context/"

instance FirebaseData MyType1 where
  getId _ = Just $ "1"
  setId a _ = a
  genId _ = return $ Just "1"

instance FirebaseContext MyType2 where
  fbCtx _ = "/MyType1Context/"

instance FirebaseData MyType2 where
  getId _ = Just $ "2"
  setId a _ = a
  genId _ = return $ Just "2"

instance FromJSON MyType2
instance ToJSON MyType2

instance FromJSON MyType1
instance ToJSON MyType1


