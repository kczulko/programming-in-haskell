module Excs1Spec where

import Excs1
import SpecHelper

spec :: Spec
spec = describe "Excs1" $ do
  it "qsorts list without duplicates" $ do
    qsort [4, 2, 5, 1] `shouldBe` ([1, 2, 4, 5] :: [Integer])
  it "qsorts list with duplicates (Excercise 5, page 13)" $ do
    qsort [4, 2, 5, 1, 1] `shouldBe` ([1, 1, 2, 4, 5] :: [Integer])

main :: IO ()
main = hspec spec
