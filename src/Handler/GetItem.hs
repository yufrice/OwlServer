module Handler.GetItem
  ( getItem
  )
where

import           Database.Persist.MongoDB
import qualified Data.Text                     as T

import           Model
import           Config
import           Utils

-- |
-- 可変長のクエリを取って商品を検索.
-- 空リストは全アイテム列挙でもいいかも.
getItem :: [T.Text] -> Owl [Entity Item]
getItem []    = runDB $ selectList [] []
getItem input = runDB $ selectList [ItemWord <-. input] []
