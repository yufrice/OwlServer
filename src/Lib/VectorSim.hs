{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}

module Lib.VectorSim where

import qualified Data.Vector                   as V

-- | uoo
similarity :: Num a => V.Vector a -> V.Vector a -> a
similarity = dotProduct

-- | Calc Lp2 norm 
-- O(n) O(n)
-- >>> l2Norm [1,2,3]
-- sqrt 14
l2Norm :: (Num a, Floating a) => V.Vector a -> a
l2Norm = sqrt . V.sum . V.map (^ 2)

-- | Calc dot product
--  O(n) O(min(mn,n))
-- >>> dotProduct [1,2,3] [3,2,1]
-- 10
dotProduct :: (Num a) => V.Vector a -> V.Vector a -> a
dotProduct = (V.sum .) . V.zipWith (*)
