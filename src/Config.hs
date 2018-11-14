{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Config where

import Control.Monad.Trans.Maybe
import Database.Persist.MongoDB
import Data.Text
import Data.Extensible
import Network (PortID(PortNumber))
import Network.Wai.Handler.Warp (Port)
import Servant (Handler)
import Control.Monad.Reader (ReaderT)

type Owl = ReaderT AppConfig Handler

data AppConfig = AppConfig
    { getPool :: ConnectionPool
    , staticDir :: FilePath
    }

type Config = Record
  '[ "listenPort" >: Port
  , "staticDir" >: FilePath
  , "database" >: DbConfig
  ]

type DbConfig = Record
  '[ "name" >: Database
  , "host" >: HostName
  , "user" >: Text
  , "pass" >: Text
  , "poolSize" >: Int
  ]

defaultConfigPath :: FilePath
defaultConfigPath = "config/settings.yaml"