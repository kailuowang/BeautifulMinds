module WeightedSlopeOneSpec where

import Test.Hspec
import WeightedSlopeOne
import qualified Data.Map as M


spec :: Spec
spec = do
  describe "predict " $ do
    it "returns exact same rating as the other user if they have the same rating over other items" $ do
      let trainedSet = update empty [M.fromList [("itemA", 2), ("itemB", 5)]]
      let result = predict trainedSet $ M.fromList [("itemA", 2)]
      result M.! "itemB" `shouldBe` 5