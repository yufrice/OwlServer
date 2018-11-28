{-# LANGUAGE OverloadedStrings #-}

module Server
  ( app
  )
where

import           Control.Monad.Reader           ( 
                                                 runReaderT
                                                )
import           Servant

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
public = login :<|> img

login :: ServerT LoginApi Owl
login = getLogin :<|> postLogin

endApi :: ServerT API Owl
endApi = itemApi :<|> vectorApi

itemApi :: ServerT ItemApi Owl
itemApi = getItem :<|> postAddItem

vectorApi :: ServerT VectorApi Owl
vectorApi = getVector

-- |
-- 画像の書き込みがDockerで閉じ込められる都合上API側で.
-- Nginx側で /imgにリバースプロキシ
img :: ServerT Raw Owl
img = serveDirectoryFileServer "./static/img"

-- |
-- デッドコード
-- static :: ServerT Raw Owl
-- static = serveDirectoryWith $ staticConfig "./static"

-- staticFiles :: [(FilePath, ByteString)]
-- staticFiles =
--   [ ("manifest.json", $(embedFile  "./static/manifest.json"))
--   , ("favicon.ico"  , $(embedFile "./static/favicon.ico"))
--   , ("index.html"   , $(embedFile "./static/index.html"))
--   ]
  -- { ssLookupFile       = ssLookupFile $ embeddedSettings $(embedDir "./static")

-- |
-- デッドコード
-- staticConfig :: FilePath -> StaticSettings
-- staticConfig path = (defaultFileServerSettings path)
--   { ssIndices          = mapMaybe toPiece ["index.html"]
--   , ssAddTrailingSlash = True
--   , ss404Handler       = Just redirectHome
--   }

-- |
-- デッドコード
-- redirectHome :: Application
-- redirectHome _ sendResponse = sendResponse $ redirectTo "/"

-- |
-- デッドコード
-- type AppServer api = ServerT api AppHandler

-- |
-- デッドコード
-- type AppHandler = ReaderT ConnectionPool Handler
