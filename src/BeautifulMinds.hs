{-# LANGUAGE OverloadedStrings #-}
module BeautifulMinds where
import WeightedSlopeOne
import Database.Neo4j
import qualified Data.HashMap.Lazy as M
import Data.Text
import Data.Int (Int64)

type FaveRating = Int64
type PhotographerId = Text
type PhotoId = Text
type FavedById = PhotographerId
type FaveRecord = (FavedById, FaveRating, PhotographerId, PhotoId)

recordFave :: FaveRecord -> IO Relationship
recordFave (favedBy, rating, photographerId, photoId) = perform $ do
  by <- savePhotographer favedBy
  photo <- savePhoto photoId photographerId
  favePhoto rating photo by


savePhotographer :: PhotographerId -> Neo4j Node
savePhotographer pid = do
  node <- createNode $ M.fromList [ "id" |: pid ]
  addLabels ["Photographer"] node
  return node

savePhoto :: PhotoId -> PhotographerId -> Neo4j Node
savePhoto photoId photographerId = do
  photo <- createNode $ M.fromList [ "id" |: photoId ]
  addLabels ["Photo"] photo
  photographer <- savePhotographer photographerId
  createRelationship "TakenBy" M.empty photo photographer
  return photo

favePhoto :: FaveRating -> Node -> Node -> Neo4j Relationship
favePhoto rating = createRelationship "Fave" (M.fromList [ "rating" |: rating])


perform :: Neo4j a -> IO a
perform = withConnection "127.0.0.1" 7474
