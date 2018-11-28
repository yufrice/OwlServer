{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Models.Post where

import           Control.Lens.TH
import           Data.Aeson              hiding ( decode
                                                , encode
                                                )
import           Data.ByteString.Lazy
import           Data.ByteString.Base64.Lazy    ( decode )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8)
import           GHC.Generics                   ( Generic(..) )

-- |
-- Post file form.
data FileInput = FileInput
  { _name :: Text -- ^ item name
  , _word :: Text -- ^ search word
  , _format :: Text -- ^ file format
  , _file :: ByteString -- ^ file binary
  , _desc :: Text -- ^ description
  }  deriving (Generic, Show)

-- |
-- lens
makeLenses ''FileInput

-- |
-- Lazy
instance FromJSON ByteString where
  parseJSON = withText "ByteString.Lazy" $ either (fail "") pure . decode . fromStrict . encodeUtf8

instance FromJSON FileInput where
  parseJSON  (Object v) =  FileInput <$> v .: "name"
    <*> v .: "word"
    <*> v .: "format"
    <*> v .: "file"
    <*> v .: "desc"
  parseJSON _ = fail ""
