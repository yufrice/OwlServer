{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Auth
  ( Authorization(..)
  , auth
  )
where

import           Data.Text                     as T
import           Database.Persist.MongoDB
import           GHC.Generics                   ( Generic )
import           Servant.API                    ( FromHttpApiData(..) )
import           Servant.Server.Internal.ServantErr
import qualified Network.HTTP.Types            as N

import           Model
import           Config
import           Utils

-- |
-- AccessToken
newtype Authorization = Authorization { getToken :: T.Text }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromHttpApiData Authorization where
  parseUrlPiece = Right . Authorization

errorResponseHeader :: N.Header
errorResponseHeader = ("WWW-Authenticate", "Basic realm=\"\"")

-- |
-- トークン検証
-- 期限切れと違法トークンの区別はつかない.
auth :: Maybe Authorization -> Owl (Either ServantErr ())
auth Nothing = return $ Left err400 { errBody    = "invalid_request."
                                    , errHeaders = [errorResponseHeader]
                                    }
auth (Just header) = case checkHeader $ T.words $ getToken header of
  Nothing -> return $ Left err400 { errBody    = "invalid_reqest."
                                  , errHeaders = [errorResponseHeader]
                                  }
  Just token -> do
    res <- runDB $ selectFirst [SessionToken ==. token] []
    case res of
      Nothing -> return $ Left err401 { errBody    = "invalid_token"
                                      , errHeaders = [errorResponseHeader]
                                      }
      Just _ -> return $ Right ()

-- |
-- Check and parse Bearer token.
-- パースするだけ.
-- 
-- >> chechHeader ["Bearer", "token"]
-- Just "token"
--
-- >> chechHeader ["Other", "token"]
-- Nothing
checkHeader :: [T.Text] -> Maybe T.Text
checkHeader [header, token] =
  if header == "Bearer" then Just token else Nothing
checkHeader _ = Nothing
