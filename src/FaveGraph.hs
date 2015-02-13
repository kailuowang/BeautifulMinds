{-# LANGUAGE OverloadedStrings #-}
module FaveGraph(recommendPhotographer, recordFave, recordFollow, FaveRecord, PhotographerId, FavedById, FaveRating, PhotoId, FollowRecord, perform) where
import WeightedSlopeOne
import Database.Neo4j
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import Data.Int (Int64)
import qualified Data.Aeson.Types as DAT
import Data.Maybe (listToMaybe)
import qualified Database.Neo4j.Transactional.Cypher as TC

type FaveRating = Int64
type PhotographerId = T.Text
type FollowerId = PhotographerId
type PhotoId = T.Text
type NodeId = T.Text
type FavedById = PhotographerId
type FaveRecord = (FavedById, FaveRating, PhotographerId, PhotoId)
type FollowRecord = (FollowerId, PhotographerId)

photographerLabel :: Label
photographerLabel = "Photographer"
photoLabel :: Label
photoLabel = "Photo"

recordFave :: FaveRecord -> IO Relationship
recordFave (favedBy, rating, photographerId, photoId) = perform $ do
  by <- savePhotographer favedBy
  photo <- savePhoto photoId photographerId
  favePhoto rating by photo

recordFollow :: FollowRecord -> IO Relationship
recordFollow (followerId, photographerId) = perform $ do
  follower <- savePhotographer followerId
  photographer <- savePhotographer photographerId
  updateOrCreate "Follow" follower photographer M.empty

recommendPhotographer :: PhotographerId -> IO [PhotographerId]
recommendPhotographer userId = perform $ do
        res <- TC.runTransaction $ do
            result <- TC.cypher "MATCH (n:Photographer)-[f1:Fave]-> (:Photo) <- [:Takes] - (:Photographer) - [:Fave] -> (:Photo) <- [:Takes] - (target: Photographer) \
                                \ WHERE n.id={photographerId} and NOT (n)-[:Follow]->(target) \
                                \ RETURN target.id, count(f1) as c \
                                \ ORDER BY c DESC" $
                                 M.fromList [("photographerId", TC.newparam userId)]
            let vals = TC.vals result
            return $ fmap head vals
        let (Right pidVals) = res
        return $ fmap getPid pidVals
        where
          getPid (DAT.String pid) = pid

findById :: Label -> NodeId -> Neo4j (Maybe Node)
findById label nid =  fmap listToMaybe $ getNodesByLabelAndProperty label $ Just ("id" |: nid)

create :: Label -> NodeId -> Neo4j Node
create label nid = do
    node <- createNode $ M.fromList [ "id" |: nid ]
    addLabels [label] node
    return node

findOrCreate :: Label -> NodeId -> Neo4j Node
findOrCreate label nid = do
  node <- findById label nid
  case node of Just n -> return n
               Nothing -> create label nid

updateOrCreate :: RelationshipType -> Node -> Node -> Properties -> Neo4j Relationship
updateOrCreate relType fromNode toNode props = do
  fromRels <- getRelationships fromNode Outgoing [relType]
  toRels <- getRelationships toNode Incoming [relType]
  case common fromRels toRels of [] -> createRelationship relType props fromNode toNode
                                 x : _ -> return x

savePhotographer :: PhotographerId -> Neo4j Node
savePhotographer = findOrCreate photographerLabel

savePhoto :: PhotoId -> PhotographerId -> Neo4j Node
savePhoto photoId photographerId = do
    maybePhoto <- findById photoLabel photoId
    case maybePhoto of Just p -> return p
                       Nothing -> createPhoto photoId photographerId

createPhoto :: PhotoId -> PhotographerId -> Neo4j Node
createPhoto photoId photographerId = do
   photo <- create photoLabel photoId
   photographer <- savePhotographer photographerId
   createRelationship "Takes" M.empty photographer photo
   return photo

favePhoto :: FaveRating -> Node -> Node -> Neo4j Relationship
favePhoto rating by photo = do
  updateOrCreate "Fave" by photo (M.fromList [ "rating" |: rating])

-- find the common elements
common :: (Eq a) => [a] -> [a] -> [a]
common xs ys = [ x | x <- xs , y <- ys, x == y]



perform :: Neo4j a -> IO a
perform = withConnection "127.0.0.1" 7474
