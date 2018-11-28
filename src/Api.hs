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
import           Data.ByteString
import           Database.Persist.MongoDB       ( Entity )

import           Model
import           Models.Result
import           Models.Post
import           Lib.Auth

-- ^
-- API endpoints.
type API = ItemApi
    :<|> VectorApi

type  APP = "api" :> API :<|> Public

-- ^
-- Public endpoints.
type Public = "api" :> LoginApi

-- ^
-- Get Search Items. 
-- Post Add Item.
type ItemApi = "item" :> QueryParams "search" Text :> Get '[JSON] [Entity Item]
    :<|> "item"
        :> Header "Authorization" Authorization
        :> ReqBody '[JSON] FileInput
        :> Post '[JSON] ()

-- ^
-- Get Search Word Vector.
type VectorApi = "vector" :> QueryParam "word" Text :> Get '[JSON] SearchResult

-- ^
-- Post Login
-- response access token.
type LoginApi = "login" :> Header "Authorization" Authorization :> Get '[JSON] ()
    :<|> "login" :> ReqBody '[JSON] User
    :> Post '[JSON] (LoginResult NoContent)

api :: Proxy APP
api = Proxy
