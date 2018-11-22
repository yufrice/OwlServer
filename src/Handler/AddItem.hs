{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Handler.AddItem
  ( postAddItem
  )
where

import           Control.Monad.Reader           ( asks
                                                , ap
                                                , liftIO
                                                , liftM2
                                                , MonadIO
                                                , MonadReader
                                                )
import           Control.Monad.Trans.Maybe
import           Control.Lens                   ( (^.) )
import           Crypto.Hash
import qualified Data.Text                     as T
import           Data.Text.Lazy.Encoding        ( encodeUtf8 )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LB
import           Database.Persist.MongoDB
import           Servant
import           System.FilePath
import           System.IO
import           System.Directory

import           Api
import           Model
import           Models.Post
import           Config
import           Lib.Auth                       ( Authorization(..)
                                                , errorResponseHeader
                                                )
import           Utils

postAddItem :: Maybe Authorization -> FileInput -> Owl ()
postAddItem Nothing _ = throwError
  $ err400 { errBody = "invalid_request", errHeaders = [errorResponseHeader] }
postAddItem (Just token) input = case checkHeader $ T.words $ getToken token of
  Nothing -> throwError err400 { errBody    = "invalid_request."
                               , errHeaders = [errorResponseHeader]
                               }
  Just token -> do
    res <- runDB $ selectFirst [SessionToken ==. token] []
    case res of
      Nothing -> throwError err401 { errBody    = "invalid_token"
                                   , errHeaders = [errorResponseHeader]
                                   }
      Just _ -> do
        img <- liftIO $ writeImage input
        runDB (insert $ item input img) >> return ()
 where
  item :: FileInput -> T.Text -> Item
  item = (<*>) (flip . liftM2 Item (^. name) (^. word)) (^. desc)

-- | Check and parse Bearer token.
-- >> chechHeader ["Bearer", "token"]
-- Just "token"
-- >> chechHeader ["Other", "token"]
-- Nothing
checkHeader :: [T.Text] -> Maybe T.Text
checkHeader [header, token] =
  if header == "Bearer" then Just token else Nothing
checkHeader _ = Nothing

-- | Write input image.
writeImage :: FileInput -> IO T.Text
writeImage input = do
  pwd <- getCurrentDirectory
  let hash = (++) (show $ h $ input ^. file) $ T.unpack $ input ^. format
  openFile (pwd </> "static" </> hash) WriteMode
    >>= (\m -> LB.hPut m (input ^. file) >> hClose m)
  return $ T.pack hash
 where
  h :: LB.ByteString -> Digest SHA3_256
  h = hashlazy
