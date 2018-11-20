{-# LANGUAGE DeriveGeneric #-}

module Models.Result where

import GHC.Generics
import Data.Aeson
import Data.Text

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