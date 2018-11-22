module Handler.GetItem where

import           Database.Persist.MongoDB
import qualified Data.Text                     as T
import           Servant

import           Api
import           Model
import           Config
import           Utils

getItem :: [T.Text] -> Owl [Entity Item]
getItem []    = runDB $ selectList [] []
getItem input = runDB $ selectList [ItemWord <-. input] []
