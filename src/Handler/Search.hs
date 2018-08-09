module Handler.Search where

import Database.Persist.MongoDB
import Servant
import Data.Text hiding (map)

import Api
import Model
import Config
import Utils

getSearch ::  (Maybe Text) -> Owl [Entity Item]
getSearch (Just input) = do
  item <- runDB $ selectList [ItemDesc ==. input] []
  return item
getSearch Nothing = return []