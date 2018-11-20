{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}

module Lib.VectorSim where

import qualified Data.Vector as V

similarity :: Num a => V.Vector a -> V.Vector a -> a
similarity = dotProduct

-- -- O(n) O(n)
l2Norm :: (Num a, Floating a) => V.Vector a -> a
l2Norm = sqrt . V.sum . V.map(\x -> x^2)

-- --  O(n) O(min(mn,n))
dotProduct :: (Num a) => V.Vector a -> V.Vector a -> a
dotProduct = (V.sum . ) . V.zipWith (*)