{-# LANGUAGE OverloadedStrings #-}
module BeautifulMinds where
import WeightedSlopeOne
import Database.Neo4j
import qualified Data.HashMap.Lazy as M
import Data.Text

myString :: String
myString = "dfdf"

savePhotographer :: Text -> IO Node
savePhotographer pid = withConnection "127.0.0.1" 7474 $ do
  createNode $ M.fromList [ "id" |: pid ]
