{-# LANGUAGE OverloadedLabels #-}

module Main
  ( main
  )
where

import           Control.Lens.Getter            ( (^.) )
import           Data.Yaml                      ( decodeFileEither
                                                , prettyPrintParseException
                                                , ParseException
                                                )
import           Database.Persist.MongoDB
                                         hiding ( master )
import           Network                        ( PortID(PortNumber) )
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger             ( withStdoutLogger )

import           Server
import           Config                         ( AppConfig(..)
                                                , Config
                                                , DbConfig
                                                , defaultConfigPath
                                                )

main :: IO ()
main = do
  config <-
    decodeFileEither defaultConfigPath :: IO (Either ParseException Config)
  case config of
    Left  e   -> putStrLn $ prettyPrintParseException e
    Right cfg -> do
      appConfig <- AppConfig <$> makePool (cfg ^. #database)
      withStdoutLogger $ \aplogger -> do
        let settings =
              setPort (cfg ^. #listenPort) $ setLogger aplogger defaultSettings
        putStrLn $ "start server listen:" ++ show (cfg ^. #listenPort)
        runSettings settings $ app $ appConfig (cfg ^. #staticDir)

-- | Created Database ConnectionPool
makePool :: DbConfig -> IO ConnectionPool
makePool config = createMongoDBPool name hostname port auth poolSize 5 3600
 where
  name     = config ^. #name
  hostname = config ^. #host
  port     = PortNumber 27017
  auth     = Just $ MongoAuth (config ^. #user) (config ^. #pass)
  poolSize = config ^. #poolSize
