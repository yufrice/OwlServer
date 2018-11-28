{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Server
  ( app
  )
where

import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                )
import           Database.Persist.MongoDB       ( ConnectionPool )
import           Servant

import           Api
import           Config
import           Data.Maybe                     ( mapMaybe )
import           Data.FileEmbed                 ( embedFile
                                                , embedDir
                                                )
import           Data.ByteString
import           Handler.GetItem
import           Handler.AddItem
import           Handler.GetVector
import           Handler.Login
import           Network.Wai.Application.Static
import           WaiAppStatic.Types
import           Network.Wai.Middleware.Vhost   ( redirectTo )

app :: AppConfig -> Application
app env = serve api $ hoistServer api (`runReaderT` env) server

server :: ServerT APP Owl
server = endApi :<|> public

public :: ServerT Public Owl
public = login

login :: ServerT LoginApi Owl
login = getLogin :<|> postLogin

endApi :: ServerT API Owl
endApi = itemApi :<|> vectorApi

itemApi :: ServerT ItemApi Owl
itemApi = getItem :<|> postAddItem

vectorApi :: ServerT VectorApi Owl
vectorApi = getVector

img :: ServerT Raw Owl
img = serveDirectoryFileServer "./static/img"

static :: ServerT Raw Owl
static = serveDirectoryWith $ staticConfig "./static"

-- staticFiles :: [(FilePath, ByteString)]
-- staticFiles =
--   [ ("manifest.json", $(embedFile  "./static/manifest.json"))
--   , ("favicon.ico"  , $(embedFile "./static/favicon.ico"))
--   , ("index.html"   , $(embedFile "./static/index.html"))
--   ]
  -- { ssLookupFile       = ssLookupFile $ embeddedSettings $(embedDir "./static")

staticConfig :: FilePath -> StaticSettings
staticConfig path = (defaultFileServerSettings path)
  { ssIndices          = mapMaybe toPiece ["index.html"]
  , ssAddTrailingSlash = True
  , ss404Handler       = Just redirectHome
  }
redirectHome :: Application
redirectHome _ sendResponse = sendResponse $ redirectTo "/"

type AppServer api = ServerT api AppHandler
type AppHandler = ReaderT ConnectionPool Handler
