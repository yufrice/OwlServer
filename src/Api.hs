{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (API(..), api)  where

import Servant
import Data.Text
import Database.Persist.MongoDB (Entity)

import Model

type API = "search" :> QueryParam "input" Text :> Get '[JSON] [Entity Item]


api :: Proxy API
api = Proxy