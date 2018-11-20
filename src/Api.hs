{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Servant
import Data.Text
import Data.ByteString
import Database.Persist.MongoDB (Entity)

import Model
import Models.Result
import Models.Post

type API = ItemApi
    :<|> VectorApi

type  APP = "api" :> API :<|> Public
type Public = "api" :> LoginApi :<|> Raw

type ItemApi = "item" :> QueryParams "search" Text :> Get '[JSON] [Entity Item]
    :<|> Header "Authorization" Text
        :> "item" :> ReqBody '[JSON] FileInput
        :> Post '[JSON] ()
type VectorApi = "vector" :> QueryParam "search" Text :> Get '[JSON] SearchResult

type LoginApi = "login" :> ReqBody '[JSON] User
    :> Post '[JSON](Headers
        '[Header "Authorization" String
        , Header "Test2" String]
    NoContent)

api :: Proxy APP
api = Proxy