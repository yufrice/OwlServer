{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import qualified Data.Text as T
import qualified Data.Text.Encoding as Te
import Data.Int
import Data.Proxy
import Database.Persist
import Database.Persist.MongoDB
import Servant
import Servant.Docs

import Api
import Config
import Model
import Models.Post
import Models.Result

main :: IO ()
main = Prelude.putStrLn $ markdown apiDoc

apiDoc :: Servant.Docs.API
apiDoc = docs api

-- ToJson
instance ToJSON Bl.ByteString where
  toJSON = object . return . ("ByteString" .=) . encode

instance ToJSON FileInput where
  toJSON (FileInput name word format file desc) = object[
      "name" .= name
      ,"word" .= word
      ,"format" .= format
      ,"file" .= file
      ,"desc" .= desc ]


-- ToParam
instance ToParam (QueryParams  "search" T.Text) where
  toParam _ = DocQueryParam "search"
                ["Word"]
                "Search Item"
                Normal

instance ToParam (QueryParam' '[Optional, Strict] "search" T.Text) where
  toParam _ = DocQueryParam "search"
                ["Word"]
                "Search Item"
                Normal

-- ToSample
instance ToSample Char where
  toSamples _ = singleSample 'a'

instance ToSample () where
  toSamples _ = noSamples

instance ToSample (Entity User) where
  toSamples _ = noSamples

instance ToSample User where
  toSamples _ = singleSample $ User "Identity" "Password"

instance ToSample (Entity Session) where
  toSamples _ = noSamples

instance ToSample Session where
  toSamples _ = noSamples

instance ToSample (Entity Item) where
  toSamples _ = noSamples

instance ToSample Item where
  toSamples _ = singleSample $ Item "ItemName" "SearchWord" "ImageSrc" "Description"

instance ToSample (Entity Vector) where
  toSamples _ = noSamples

instance ToSample Vector where
  toSamples _ = noSamples

instance ToSample FileInput where
  toSamples _ = singleSample
    $ FileInput "ItemName" "SearchWord" "FileFormat" (Bl.fromStrict $ Te.encodeUtf8 "File") "Description"

instance ToSample ResultWord where
  toSamples _ = singleSample $ ResultWord "ResultWord" 1.0

instance ToSample SearchResult where
  toSamples _ = singleSample $ SearchResult [ResultWord "ListResultWord" 0.1]