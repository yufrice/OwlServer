{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Data.Aeson              hiding ( encode )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as Bl
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as Te
import           Data.Int
import           Data.Proxy
import           Database.Persist
import           Database.Persist.MongoDB
import           Servant
import           Servant.Docs

import           Api
import           Config
import           Model
import           Models.Post
import           Models.Result

main :: IO ()
main = writeFile "docs/API/index.md" $ markdown apiDoc

apiDoc :: Servant.Docs.API
apiDoc = docs api

-- ToJson
instance ToJSON Bl.ByteString where
  toJSON = toJSON . Te.decodeUtf8 . Bl.toStrict

instance ToJSON FileInput where
  toJSON (FileInput name word format file desc) = object[
      "name" .= name
      ,"word" .= word
      ,"format" .= format
      ,"file" .= file
      ,"desc" .= desc ]


-- Item
instance ToParam (QueryParams "search" T.Text) where
  toParam _ = DocQueryParam "search" ["word0", "word1", "wordN"] "Search Item" List

-- instance ToCapture (Capture "fileInput" FileInput) where
--   toCapture _ = DocCapture "fileInput" "file input"

-- Vector
instance ToParam (QueryParam' '[Optional, Strict] "word" T.Text) where
  toParam _ = DocQueryParam "word" ["word0", "word1", "wordN"] "Search Vector" Normal

-- ToSample
instance ToSample () where
  toSamples _ = singleSample ()

instance ToSample Char where
  toSamples _ = singleSample 'a'

instance ToSample Int where
  toSamples _ = singleSample 0

instance ToSample B.ByteString where
  toSamples _ = singleSample "test"

instance ToSample T.Text where
  toSamples _ = singleSample "test"

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

-- sampleFile = FileInput "ItemName" "SearchWord" "FileFormat" "File" "Description"
instance ToSample FileInput where
  toSamples _ = noSamples
  -- toSamples _ = singleSample sampleFile

instance ToSample SearchResult where
  toSamples _ = singleSample $ SearchResult [
    ResultWord "ResultWord0" 1.0
    ,ResultWord "ResultWord1" 0.1]
