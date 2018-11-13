{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model (
    module Model
    , module Models.Vec
) where

import Data.ByteString (ByteString)
import Data.Text
import Database.Persist.Quasi
import Database.Persist.MongoDB hiding (master)
import Database.Persist.TH
import Prelude (Bool(..))
import Servant
import Language.Haskell.TH.Syntax
import Models.Vec
import Utils

share [mkPersist (mkPersistSettings (ConT ''MongoContext)){mpsGenerateLenses = True}]
  $(persistFileWith upperCaseSettings "config/models")

data FileInput = FileInput
  { name :: Text
  , desc :: Text
  , file :: ByteString
  }