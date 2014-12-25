module BeautifulMindsSpec where

import Test.Hspec
import BeautifulMinds


spec :: Spec
spec = do
  describe "myString" $ do
    it "returns dfdf" $ do
      myString `shouldBe` "dfdf"