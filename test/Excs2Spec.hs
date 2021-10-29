module Excs2Spec where

import Excs2
import SpecHelper

spec :: Spec
spec = describe "Excs2" $ do
  it "init returns list without the last element" $ do
    let
      param = [4, 2, 5, 1] :: [Integer]
      expected = [4, 2, 5] :: [Integer]
    init1 param `shouldBe` expected
    init2 param `shouldBe` expected
  it "last returns ultimate list elem" $ do
    let
      param = [4, 2, 5, 1] :: [Integer]
      expected = 1 :: Integer
    last1 param `shouldBe` expected
    last2 param `shouldBe` expected      

main :: IO ()
main = hspec spec
