module Handler.DeleteItem where

import           Control.Monad.Reader           ( liftIO )
import           Data.Text
import           Database.Persist.MongoDB
import           System.FilePath
import           System.Directory

import           Model
import           Config
import           Utils

deleteAddItem :: Maybe Text -> Owl ()
deleteAddItem Nothing   = return ()
deleteAddItem (Just id) = do
  res <- runDB $ selectFirst [ItemImage ==. id] []
  case res of
    Nothing             -> return ()
    Just (Entity key _) -> runDB $ delete key
