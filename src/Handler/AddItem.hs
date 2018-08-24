{-# LANGUAGE FlexibleContexts #-}

module Handler.AddItem where

import Crypto.Hash (digestFromByteString, Digest, SHA256)
import Control.Monad.Reader (asks, liftIO, MonadIO, MonadReader)
import Data.Text hiding (map)
import Data.ByteString (ByteString)
import Database.Persist.MongoDB
import Servant

import Api
import Model
import Config
import Utils

postAddItem :: FileInput -> Owl ()
postAddItem input = return ()

imageFilePath :: (MonadReader AppConfig m, MonadIO m) => m FilePath
imageFilePath = asks staticDir

-- writeImage :: FileInput -> IO (Maybe (Digest SHA256))
-- writeImage input  = do
--   hash <-return $ hashFromFile $ file input
--   case hash of
--     Just hash -> do
--       uri <- return $ show hash <.> i
--       return $ Nothing
--     Nothing -> reutrn $ Nothing


hashFromFile :: ByteString -> Maybe (Digest SHA256)
hashFromFile = digestFromByteString