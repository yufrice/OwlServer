{-# LANGUAGE ScopedTypeVariables #-}

module Handler.GetVector where

import Database.Persist.MongoDB
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Ord (comparing)
import Data.List (null, sortBy)
import Control.Lens ((^.))
import Servant

import Api
import Model
import Config
import Models.Result
import Lib.VectorSim
import Utils

getVector :: Maybe T.Text -> Owl SearchResult
getVector (Just input) = do
    res <- runDB $ selectList [VectorWord ==. input] []
    case null res of
        True -> return $ SearchResult []
        False -> do
            vecs <- runDB $ selectList [VectorWord !=. input] []
            _sim <- return $ mostSim (f res ^. vectorVector) vecs
            word <- return $ map (\(Entity _ x) -> x ^. vectorWord) vecs
            return $ SearchResult $ (sortBy.flip) (comparing sim) $ filter (\x -> minSim < sim x) $ zipWith ResultWord word _sim
    where
        f ((Entity _ val):_) = val

getVector Nothing =  return $ SearchResult []

minSim :: Double
minSim = 0.3

mostSim :: V.Vector Double -> [Entity Vector] -> [Double]
mostSim _ [] = []
mostSim center ((Entity _ vec) : xs) = [similarity center $ vec ^. vectorVector] ++ mostSim center xs