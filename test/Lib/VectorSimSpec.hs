module Lib.VectorSimSpec
  ( spec
  )
where


import           Lib.VectorSim

import           Test.Hspec
import           Test.QuickCheck
import           Test.Hspec.QuickCheck          ( prop )
import           Data.Vector
import           Control.Monad                  ( liftM2 )

testVector0 :: Vector Double
testVector0 = fromList [ x * 2 | x <- [1 .. 10] ]
testVector1 :: Vector Double
testVector1 = fromList [1 .. 1000]

testNormalize0Vector :: Vector Double
testNormalize0Vector = Data.Vector.map (/ l2Norm testVector0) testVector0
testNormalize1Vector :: Vector Double
testNormalize1Vector = Data.Vector.map (/ l2Norm testVector1) testVector1

epsilonDouble :: Double
epsilonDouble = 1e-12

spec :: Spec
spec = do
  describe "Dot product" $ do
    it "10dim" $ dotProduct testVector0 testVector0 `shouldBe` 1540
    it "1000dim" $ dotProduct testVector1 testVector1 `shouldBe` 333833500
  describe "L2 Norm" $ do
    it "10dim" $ l2Norm testVector0 `shouldBe` 2 * sqrt 385
    it "1000dim" $ l2Norm testVector1 `shouldBe` 10 * sqrt 3338335
  describe "Similarity" $ it "Near" $ do
    similarity testNormalize0Vector testNormalize0Vector `shouldBe` 1
    similarity testNormalize1Vector testNormalize1Vector
      `shouldSatisfy` liftM2 (&&) (1 + epsilonDouble >=) (1 - epsilonDouble <=)
