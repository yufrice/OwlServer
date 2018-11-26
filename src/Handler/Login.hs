{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Login
  ( postLogin
  , getLogin
  )
where

import           Control.Lens                   ( (^.) )
import qualified Crypto.BCrypt                 as C
import           Crypto.Hash
import           Control.Monad.IO.Class         ( liftIO )
import           Database.Persist.MongoDB
import qualified Data.Text                     as T
import qualified Data.ByteArray                as Ba
import qualified Data.ByteString               as B
import qualified Data.Time.Clock               as DT
import           Servant
import           System.Entropy

import           Model
import           Models.Result
import           Config
import           Lib.Auth
import           Utils

getLogin :: Maybe Authorization -> Owl ()
getLogin token = case return $ auth token of
  Left  err -> throwError err
  Right _   -> return ()

postLogin :: User -> Owl (LoginResult NoContent)
postLogin user = do
  res <- runDB $ selectFirst [UserIdent ==. user ^. userIdent] []
  case res of
    Nothing -> throwError err401
    Just r  ->
      if C.validatePassword (entityVal r ^. userPassword) $ user ^. userPassword
        then
          (do
            token <- liftIO $ makeToken 5
            time  <- liftIO DT.getCurrentTime
            let session = Session token time
            runDB $ insert session
            return
              $ addHeader token
              $ addHeader "Bearer"
              $ addHeader 3600
              $ addHeader "not implemented" NoContent
          )
        else throwError err401

-- |
-- Create a random token.
-- 
-- >>> makeToken 10
-- random token
makeToken :: Int -> IO T.Text
makeToken i = do
  seed <- getEntropy i
  return $ T.pack $ show (hash seed :: Digest SHA256)
