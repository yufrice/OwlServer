{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Handler.AddItem
  ( postAddItem
  )
where

import           Control.Monad.Reader           (
                                                 liftIO
                                                , liftM2
                                                )
import           Control.Lens                   ( (^.) )
import           Crypto.Hash
import qualified Data.Text                     as T
import qualified Data.ByteString.Lazy          as LB
import           Database.Persist.MongoDB
import           Servant
import           System.FilePath
import           System.IO
import           System.Directory

import           Model
import           Models.Post
import           Config
import           Lib.Auth                       ( Authorization(..)
                                                , auth
                                                )
import           Utils

-- |
-- トークンをチェックしてDBに登録.
-- あとで画像か否かのチェックぐらいは書く.
postAddItem :: Maybe Authorization -> FileInput -> Owl ()
postAddItem token input = case return $ auth token of
  Left  err -> throwError err
  Right _   -> do
    img <- liftIO $ writeImage input
    runDB (insert $ item input (T.append "img/" img)) >> return ()
 where
  item :: FileInput -> T.Text -> Item
  item = (<*>) (flip . liftM2 Item (^. name) (^. word)) (^. desc)

-- |
-- Write input image.
writeImage :: FileInput -> IO T.Text
writeImage input = do
  pwd <- getCurrentDirectory
  let _hash = (++) (show $ h $ input ^. file) $ T.unpack $ input ^. format
  openFile (pwd </> "static/img" </> _hash) WriteMode
    >>= (\m -> LB.hPut m (input ^. file) >> hClose m)
  return $ T.pack _hash
 where
  h :: LB.ByteString -> Digest SHA3_256
  h = hashlazy
