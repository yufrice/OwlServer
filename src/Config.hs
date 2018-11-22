{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Config
  ( AppConfig(..)
  , Config
  , DbConfig
  , defaultConfigPath
  , Owl
  )
where

import           Control.Monad.Trans.Maybe
import           Control.Monad.Reader           ( ReaderT )
import           Database.Persist.MongoDB
import           Data.Text
import           Data.Extensible
import           Network                        ( PortID(PortNumber) )
import           Network.Wai.Handler.Warp       ( Port )
import           Servant                        ( Handler )

-- ^ Wrap Handler in ReadMonad.
type Owl = ReaderT AppConfig Handler

-- ^ Context Settings.
data AppConfig = AppConfig
    { getPool :: ConnectionPool
    , staticDir :: FilePath
    }

-- ^ General configs.
type Config = Record
  '[ "listenPort" >: Port
  , "staticDir" >: FilePath
  , "database" >: DbConfig
  ]

-- ^ Database Configs.
type DbConfig = Record
  '[ "name" >: Database
  , "host" >: HostName
  , "user" >: Text
  , "pass" >: Text
  , "poolSize" >: Int
  ]

-- ^ Config file path.
defaultConfigPath :: FilePath
defaultConfigPath = "config/settings.yaml"
