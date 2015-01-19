{-# LANGUAGE OverloadedStrings #-}

module BeautifulMindsSpec where


import Test.Hspec
import BeautifulMinds
import Database.Neo4j
import qualified Data.HashMap.Lazy as M

spec :: Spec
spec = do
  describe "store neo4j " $ do
    it "store a vertice as photographer" $ do
      node <- savePhotographer "23"
      let pid = (getNodeProperties node) M.! "id"
      pid `shouldBe` (ValueProperty $ TextVal "23")
