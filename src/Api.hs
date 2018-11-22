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
import Lib.Auth

type API = ItemApi
    :<|> VectorApi

type  APP = "api" :> API :<|> Public
type Public = "api" :> LoginApi :<|> Raw

type ItemApi = "item" :> QueryParams "search" Text :> Get '[JSON] [Entity Item]
    :<|> "item"
        :> Header "Authorization" Authorization
        :> ReqBody '[JSON] FileInput
        :> Post '[JSON] ()
type VectorApi = "vector" :> QueryParam "word" Text :> Get '[JSON] SearchResult

type LoginApi = "login" :> ReqBody '[JSON] User
    :> Post '[JSON] (LoginResult NoContent)

api :: Proxy APP
api = Proxy