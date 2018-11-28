{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model
    ( module Model
    , module Models.Vec
    )
where

import           Data.Aeson              hiding ( decode
                                                , encode
                                                )
import           Data.ByteString                ( ByteString )
import           Data.Text
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.Time                      ( UTCTime(..) )
import           Database.Persist.Quasi
import           Database.Persist.MongoDB
                                         hiding ( master )
import           Database.Persist.TH
import           Servant
import           Language.Haskell.TH.Syntax
import           Models.Vec

instance ToJSON ByteString where
  toJSON =  toJSON . decodeUtf8

instance FromJSON ByteString where
    parseJSON = withText "ByteString" $ pure . encodeUtf8

share [mkPersist (mkPersistSettings (ConT ''MongoContext)){mpsGenerateLenses = True}]
  $(persistFileWith upperCaseSettings "config/models")
