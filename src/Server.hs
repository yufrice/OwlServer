module Server where

import Servant
import Control.Monad.Reader (ReaderT, runReaderT)
import Database.Persist.MongoDB (ConnectionPool)

import Api
import Config
import Handler.Search

app :: AppConfig -> Application
app env = serve api $ hoistServer api (flip runReaderT env) server

server :: ServerT API Owl
server = getSearch

type AppServer api = ServerT api AppHandler
type AppHandler = ReaderT ConnectionPool Handler