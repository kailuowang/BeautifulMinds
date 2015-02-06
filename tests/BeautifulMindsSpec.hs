{-# LANGUAGE OverloadedStrings #-}

module BeautifulMindsSpec where


import Test.Hspec
import BeautifulMinds
import Database.Neo4j
import qualified Data.HashMap.Lazy as M

spec :: Spec
spec = do
  describe "record fave " $ do
    it "store the relationship" $ do
      faveRel <- recordFave ("aUser", 3, "aPhotographer", "aPhoto")
      let rating = getRelProperties faveRel M.! "rating"
      rating `shouldBe` (ValueProperty $ IntVal 3)
      (user, photo) <-  perform $ do
        from <- getRelationshipFrom faveRel
        to <- getRelationshipTo faveRel
        return (from, to)
      getNodeProperties user M.! "id" `shouldBe` (ValueProperty $ TextVal "aUser")
      getNodeProperties photo M.! "id" `shouldBe` (ValueProperty $ TextVal "aPhoto")

  describe "recommend photographer" $ do
    it "returns only unfollowed user" $ do
      _ <- recordFave ("alf", 3, "jon", "aPhotob")
      _ <- recordFave ("jon", 3, "already", "1aPhoto")
      _ <- recordFave ("jon", 3, "new", "aPho2to")
      _ <- recordFollow ("alf", "already")
      recommended <- recommendPhotographer "alf"
      recommended `shouldBe` ["new"]