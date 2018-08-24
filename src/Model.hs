{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import Data.ByteString (ByteString)
import Database.Persist.TH
import Database.Persist.Quasi
import Database.Persist.MongoDB hiding (master)
import Language.Haskell.TH.Syntax
import Data.Text
import Servant

share [mkPersist (mkPersistSettings (ConT ''MongoContext))]
  $(persistFileWith upperCaseSettings "config/models")

data FileInput = FileInput
  { name :: Text
  , desc :: Text
  , file :: ByteString
  }