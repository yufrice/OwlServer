{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}

module Lib.VectorSim where

import qualified Data.Vector                   as V

-- |
-- ベクトルの次元数まで型定義したかったがチャーチ数的に300～再帰でコンパイル時間が爆発する.
similarity :: Num a => V.Vector a -> V.Vector a -> a
similarity = dotProduct

-- |
-- Calc Lp2 norm.
-- O(n) O(n)
-- 事前に正規化しているのでデッドコード.
-- 
-- >>> l2Norm [1,2,3]
-- sqrt 14
l2Norm :: (Num a, Floating a) => V.Vector a -> a
l2Norm = sqrt . V.sum . V.map (** 2)

-- |
-- Calc dot product.
--  O(n) O(min(mn,n))
-- 
-- >>> dotProduct [1,2,3] [3,2,1]
-- 10
dotProduct :: (Num a) => V.Vector a -> V.Vector a -> a
dotProduct = (V.sum .) . V.zipWith (*)
