{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Login
  ( login
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
import           Utils

login :: User -> Owl (LoginResult NoContent)
login user = do
  res <- runDB $ selectFirst [UserIdent ==. user ^. userIdent] []
  case res of
    Nothing -> throwError err401
    Just r  -> do
      hash <-
        liftIO
        $  C.hashPasswordUsingPolicy C.slowerBcryptHashingPolicy
        $  user
        ^. userPassword
      case hash of
        Nothing -> throwError err401
        Just h  -> if C.validatePassword h $ entityVal r ^. userPassword
          then throwError err401
          else
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

makeToken :: Int -> IO T.Text
makeToken i = do
  seed <- getEntropy i
  return $ T.pack $ show (hash seed :: Digest SHA256)
