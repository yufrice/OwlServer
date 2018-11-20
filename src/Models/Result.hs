{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Models.Result where

import Data.Aeson
import Data.Text
import GHC.Generics
import Servant

data SearchResult = SearchResult {
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
            '[Header "access_token" String
            , Header "token_type" String
            , Header "expires_in" String
            , Header "refresh_token" String]