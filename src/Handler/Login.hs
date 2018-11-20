{-# LANGUAGE DataKinds #-}

module Handler.Login where

import Control.Lens ((^.))
import Database.Persist.MongoDB
import qualified Data.Text as T
import Servant

import Model
import Models.Result
import Config
import Utils

login :: User -> Owl (LoginResult NoContent)
login user = do
  res <- runDB $ selectFirst [UserIdent ==. user ^. userIdent] []
  case res of
    Nothing -> throwError $ err401
    Just _ -> return
      $ addHeader "token"
      $ addHeader "Bearer"
      $ addHeader "3600"
      $ addHeader "token"
      $ NoContent