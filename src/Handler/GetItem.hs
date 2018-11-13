module Handler.GetItem where

import Database.Persist.MongoDB
import qualified Data.Text as T
import Servant

import Api
import Model
import Config
import Utils

getItem :: Maybe T.Text -> Owl [Entity Item]
getItem (Just input) = runDB $ selectList [ItemName ==. input] []

getItem Nothing = runDB $ selectList [] []