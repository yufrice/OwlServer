{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

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
import           Servant
import           Servant.API                    ( ToHttpApiData(..) )

newtype SearchResult = SearchResult {
    result :: [ResultWord]
} deriving (Generic, Show)

data ItemPostResult = ItemPostResult {
    code :: Int
    , message :: String
} deriving (Generic, Show)

data ResultWord = ResultWord {
    word :: Text
    , sim :: Double
} deriving (Generic, Show)

instance ToJSON SearchResult
instance ToJSON ResultWord
instance ToJSON ItemPostResult

type LoginResult = Headers
            '[Header "access_token" Text
            , Header "token_type" ByteString
            , Header "expires_in" Int
            , Header "refresh_token" ByteString]

instance ToHttpApiData ByteString where
    toQueryParam = decodeUtf8
