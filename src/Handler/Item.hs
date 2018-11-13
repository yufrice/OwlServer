module Handler.Item where

import Database.Persist.MongoDB
import qualified Data.Text as T
import Servant

import Api
import Model
import Config
import Utils

getSearch :: Maybe T.Text -> Owl [Entity Item]
getSearch (Just input) = runDB $ selectList [ItemName ==. input] []

getSearch Nothing = runDB $ selectList [] []