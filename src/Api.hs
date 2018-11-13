{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (API(..), api)  where

import Servant
import Data.Text
import Database.Persist.MongoDB (Entity)

import Model
import Models.Result

type API = ItemApi
    :<|> VectorApi

type ItemApi = "item" :> QueryParam "search" Text :> Get '[JSON] [Entity Item]
type VectorApi = "vector" :> QueryParam "search" Text :> Get '[JSON] SearchResult



api :: Proxy API
api = Proxy