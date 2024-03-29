{-# LANGUAGE FlexibleContexts #-}

module Utils
  ( runDB
  )
where

import           Control.Monad.Reader           ( asks
                                                , liftIO
                                                , MonadIO
                                                , MonadReader
                                                )
import           Control.Monad.Trans.Resource   ( ResourceT
                                                , runResourceT
                                                )
import           Database.Persist.MongoDB

import           Config                         ( AppConfig(..) )

type MongoPersistM = Action (ResourceT IO)

-- |
-- Connecting Database Utility.
runDB :: (MonadReader AppConfig m, MonadIO m) => MongoPersistM a -> m a
runDB query = do
  pool <- asks getPool
  liftIO $ runResourceT $ runMongoDBPool master query pool
