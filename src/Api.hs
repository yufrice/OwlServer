{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( API
    , APP
    , api
    , ItemApi
    , LoginApi
    , Public
    , VectorApi
    )
where

import           Servant
import           Data.Text
import           Database.Persist.MongoDB       ( Entity )

import           Model
import           Models.Result
import           Models.Post
import           Lib.Auth

-- |
-- APP endpoint.
type  APP = "api" :> API :<|> Public

-- |
-- API endpoint.
type API = ItemApi
    :<|> VectorApi

-- |
-- Public endpoint.
-- itemのpostしかauthかけてないのでこのドメイン分けはおかしい
type Public = "api" :> LoginApi :<|> Raw

-- |
-- Get Search Items.
-- Post Add Item.
type ItemApi = "item" :> QueryParams "search" Text :> Get '[JSON] [Entity Item]
    :<|> "item"
        :> Header "Authorization" Authorization
        :> ReqBody '[JSON] FileInput
        :> Post '[JSON] ()

-- |
-- Get Search Word Vector.
type VectorApi = "vector" :> QueryParam "word" Text :> Get '[JSON] SearchResult

-- |
-- Get Session Token check.
-- Post Login
-- response access token.
type LoginApi = "login" :> Header "Authorization" Authorization :> Get '[JSON] ()
    :<|> "login"
        :> ReqBody '[JSON] User
        :> Post '[JSON] (LoginResult NoContent)

api :: Proxy APP
api = Proxy
