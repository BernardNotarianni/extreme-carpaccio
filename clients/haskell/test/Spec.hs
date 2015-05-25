module Main where

import Test.Hspec
import Model

main :: IO ()
main = hspec $ do

  describe "computeTotal" $ do

    it "should be sum of quantities * price" $ do
      let order = Order [2.0] [3] "" ""
       in computeTotal order `shouldBe` Quantity 6.0

    it "should apply VAT of 20% for country FR" $ do
      let order = Order [2.0] [3] "FR" ""
       in computeTotal order `shouldBe` Quantity 7.2

    it "sould apply reduction when total >= 1000" $ do
      let order = Order [500.0] [2] "FR" "STANDARD"
       in computeTotal order `shouldBe` Quantity (1000.0*1.2*0.97)

    it "sould apply half price reduction when enabled" $ do
      let order = Order [75.73,98.99] [1,5] "PT" "HALF PRICE"
       in computeTotal order `shouldBe` Quantity 350.97
