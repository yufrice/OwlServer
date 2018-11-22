{-# LANGUAGE ScopedTypeVariables #-}

module Handler.GetVector where

import           Database.Persist.MongoDB
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Data.Ord                       ( comparing )
import           Data.List                      ( null
                                                , sortBy
                                                )
import           Control.Lens                   ( (^.) )
import           Servant

import           Api
import           Model
import           Config
import           Models.Result
import           Lib.VectorSim
import           Utils

getVector :: Maybe T.Text -> Owl SearchResult
getVector (Just input) = do
    res <- runDB $ selectList [VectorWord ==. input] []
    if null res
        then return $ SearchResult []
        else
            (do
                vecs <- runDB $ selectList [VectorWord !=. input] []
                let _sim =
                        mostSim ((^.) (entityVal $ head res) vectorVector) vecs
                let word = map ((^. vectorWord) . entityVal) vecs
                return
                    $ SearchResult
                    $ (sortBy . flip) (comparing sim)
                    $ filter ((minSim <) . sim)
                    $ zipWith ResultWord word _sim
            )

getVector Nothing = return $ SearchResult []

minSim :: Double
minSim = 0.3

mostSim :: V.Vector Double -> [Entity Vector] -> [Double]
mostSim _ [] = []
mostSim center (Entity _ vec : xs) =
    similarity center (vec ^. vectorVector) : mostSim center xs
