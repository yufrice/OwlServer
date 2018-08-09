{-# LANGUAGE FlexibleContexts #-}

module Handler.AddItem where

import Crypto.Hash (Digest, SHA256)
import Control.Monad.Reader (asks, liftIO, MonadIO, MonadReader)
import Data.Text hiding (map)
import Database.Persist.MongoDB
import Servant

import Api
import Model
import Config
import Utils

postAddItem :: Item -> Owl ()
postAddItem item = return ()

imageFilePath :: (MonadReader AppConfig m, MonadIO m) => m FilePath
imageFilePath = asks staticDir