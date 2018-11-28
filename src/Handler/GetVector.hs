{-# LANGUAGE ScopedTypeVariables #-}

module Handler.GetVector
    ( getVector
    )
where

import           Database.Persist.MongoDB
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Data.Ord                       ( comparing )
import           Data.List                      ( null
                                                , sortBy
                                                )
import           Control.Lens                   ( (^.) )

import           Model
import           Config
import           Models.Result                  ( SearchResult(..)
                                                , ResultWord(..)
                                                )
import           Lib.VectorSim
import           Utils

-- |
-- クエリから近い単語と距離のタプルを返す.
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
                let _word = map ((^. vectorWord) . entityVal) vecs
                return
                    $ SearchResult
                    $ (sortBy . flip) (comparing sim)
                    $ filter ((minSim <) . sim)
                    $ zipWith ResultWord _word _sim
            )
getVector Nothing = return $ SearchResult []

-- |
-- Filtering minimum similarity.
-- あとでコンパイル時じゃなくて動的に変更できるようにする.
minSim :: Double
minSim = 0.3

-- |
-- Mapping vector from lists.
mostSim :: V.Vector Double -> [Entity Vector] -> [Double]
mostSim _ [] = []
mostSim center (Entity _ vec : xs) =
    similarity center (vec ^. vectorVector) : mostSim center xs
