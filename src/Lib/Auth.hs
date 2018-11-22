{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Auth
  ( Authorization(..)
  , errorResponseHeader
  )
where

import           Data.Text
import           GHC.Generics                   ( Generic )
import           Servant.API                    ( FromHttpApiData(..) )
import qualified Network.HTTP.Types            as N

newtype Authorization = Authorization { getToken :: Text }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromHttpApiData Authorization where
  parseUrlPiece = Right . Authorization

errorResponseHeader :: N.Header
errorResponseHeader = ("WWW-Authenticate", "Basic realm=\"\"")
