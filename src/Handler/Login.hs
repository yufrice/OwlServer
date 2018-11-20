{-# LANGUAGE DataKinds #-}

module Handler.Login where

import Control.Lens ((^.))
import Database.Persist.MongoDB
import qualified Data.Text as T
import Servant

import Model
import Config
import Utils

login :: User -> Owl (Headers
  '[Header "Authorization" String
  , Header "Test2" String] NoContent)
login user = do
  res <- runDB $ selectList [UserIdent ==. user ^. userIdent] []
  case Prelude.null res of
    True -> return $ addHeader "Bearer <Token>" $ addHeader "v" $ NoContent
    False -> return $ addHeader "a" $ addHeader "v" $ NoContent