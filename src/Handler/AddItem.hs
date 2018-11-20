{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Handler.AddItem where

import Control.Monad.Reader (asks, ap, liftIO, liftM2,MonadIO, MonadReader)
import Control.Monad.Trans.Maybe
import Control.Lens ((^.))
import Crypto.Hash
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Database.Persist.MongoDB
import Servant
import Servant.Multipart
import System.FilePath
import System.IO
import System.Directory
import qualified Network.HTTP.Types as N

import Api
import Model
import Models.Post
import Config
import Utils

postAddItem :: Maybe T.Text -> FileInput -> Owl ()
postAddItem Nothing _ = throwError $ err400 { errBody = "invalid_request"
                                            , errHeaders = [errorResponseHeader] }
postAddItem (Just token) input = do
  case checkHeader $ T.words token of
    Nothing -> throwError err400 { errBody =  "invalid_request."
                                 , errHeaders = [errorResponseHeader] }
    Just token -> do
      res <- runDB $ selectFirst [SessionToken ==. token] []
      case res of
        Nothing -> throwError err401 { errBody = "invalid_token"
                                     , errHeaders = [errorResponseHeader] }
        Just _ -> do
          img <- liftIO $ writeImage input
          (runDB $ insert $ item input img) >> return ()
  where
    item :: FileInput -> T.Text -> Item
    item = (<*>) (flip . liftM2 Item (^. name) (^. word)) (^. desc)

errorResponseHeader :: N.Header
errorResponseHeader = ("WWW-Authenticate", "Basic realm=\"\"")

checkHeader :: [T.Text] -> Maybe T.Text
checkHeader [header, token] = if header == "Bearer" then Just token else Nothing
checkHeader _ = Nothing



writeImage :: FileInput -> IO T.Text
writeImage input = do
  pwd <- getCurrentDirectory
  hash <- return $ (++) (show $ h $ input ^. file) $ T.unpack $ input ^. format
  openFile (pwd </> "static" </> hash) WriteMode
    >>= (\m -> (LB.hPut m $ input ^. file) >> hClose m)
  return $ T.pack hash
  where
    h :: LB.ByteString -> Digest SHA3_256
    h = hashlazy