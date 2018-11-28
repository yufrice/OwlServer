{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Models.Result
    ( SearchResult(..)
    , ItemPostResult
    , LoginResult
    , ResultWord(..)
    )
where

import           Data.Aeson
import           Data.ByteString                ( ByteString )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.Text
import           GHC.Generics
import           Servant.API                    ( ToHttpApiData(..), Headers )
import  Servant.API.Header (Header)

-- |
-- Item search result.
newtype SearchResult = SearchResult {
    result :: [ResultWord]
} deriving (Generic, Show)

-- |
-- Add item response code.
data ItemPostResult = ItemPostResult {
    code :: Int -- ^ response code
    , message :: String -- ^ response message
} deriving (Generic, Show)

-- |
-- Vector search result.
-- tuple(Word, Similarity)
data ResultWord = ResultWord {
    word :: Text    -- ^ word
    , sim :: Double -- ^ sim
} deriving (Generic, Show)

instance ToJSON SearchResult
instance ToJSON ResultWord
instance ToJSON ItemPostResult

-- |
-- Login responce.
type LoginResult = Headers
            '[Header "access_token" Text
            , Header "token_type" ByteString
            , Header "expires_in" Int
            , Header "refresh_token" ByteString]

instance ToHttpApiData ByteString where
    toQueryParam = decodeUtf8
