module Server where

import           Servant
import           Servant.Server
import           Servant.Server.StaticFiles
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                )
import           Database.Persist.MongoDB       ( ConnectionPool )

import           Api
import           Config
import           Handler.GetItem
import           Handler.AddItem
import           Handler.GetVector
import           Handler.Login

app :: AppConfig -> Application
app env = serve api $ hoistServer api (`runReaderT` env) server

server :: ServerT APP Owl
server = endApi :<|> public

public :: ServerT Public Owl
public = login :<|> static

endApi :: ServerT API Owl
endApi = itemApi :<|> vectorApi

itemApi :: ServerT ItemApi Owl
itemApi = getItem :<|> postAddItem

vectorApi :: ServerT VectorApi Owl
vectorApi = getVector

static :: ServerT Raw Owl
static = serveDirectoryFileServer "../owl-webapp/build"

type AppServer api = ServerT api AppHandler
type AppHandler = ReaderT ConnectionPool Handler
