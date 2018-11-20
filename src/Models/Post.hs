{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Post where

import Control.Lens.TH
import Data.Aeson hiding (decode)
import Data.ByteString.Lazy
import Data.ByteString.Base64.Lazy (decode)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic(..))

data FileInput = FileInput
  { _name :: Text
  , _word :: Text
  , _format :: Text
  , _file :: ByteString
  , _desc :: Text
  }  deriving (Generic, Show)

makeLenses ''FileInput

instance FromJSON ByteString where
  parseJSON = withText "ByteString" $ either (fail "") pure . id . decode . fromStrict . encodeUtf8
  -- parseJSON (Text str) = pure $ (either (const "") id . decode . encodeUtf8 . pack) str

instance FromJSON FileInput where
  parseJSON  (Object v) =  FileInput <$> v .: "name"
    <*> v .: "word"
    <*> v .: "format"
    <*> v .: "file"
    <*> v .: "desc"