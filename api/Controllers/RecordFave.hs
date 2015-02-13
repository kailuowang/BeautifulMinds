{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Controllers.RecordFave(postFaveRecord, postFollowRecord, getRecommendation) where

import           FaveGraph
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import           Data.Aeson.TH            (deriveJSON, defaultOptions)
import           Data.Int                 (Int64)
import           Data.Aeson               (Value, encode, object, (.=), FromJSON, ToJSON)
import           Data.Aeson.Parser        (json)
import           Data.Aeson.Types
import           Network.Wai
import           Network.HTTP.Types       (status200, status400)
import           Data.ByteString.Lazy     (ByteString)


data FaveRecordRequest =
  FaveRecordRequest {
    favedBy :: PhotographerId,
    rating :: FaveRating,
    photographer :: PhotographerId,
    photo  :: PhotoId

   } deriving (Show)

data FollowRecordRequest =
  FollowRecordRequest {
    follower :: PhotographerId,
    followed :: PhotographerId
   } deriving (Show)

$(deriveJSON defaultOptions ''FaveRecordRequest)
$(deriveJSON defaultOptions ''FollowRecordRequest)

postFaveRecord :: Value -> [T.Text] -> IO ByteString
postFaveRecord value _ = do
        let (Success frr) = fromJSON value
        relationship <- recordFave $ toFaveRecord frr
        return $ encode $ M.fromList [("status" :: T.Text, "success":: T.Text)]

postFollowRecord :: Value -> [T.Text] -> IO ByteString
postFollowRecord value _ = do
        let (Success frr) = fromJSON value
        relationship <- recordFollow $ toFollowRecord frr
        return $ encode $ M.fromList [("status" :: T.Text, "success":: T.Text)]

getRecommendation :: Value -> [T.Text] -> IO ByteString
getRecommendation _ (forId : _) = do
        recommended <- recommendPhotographer forId
        return $ encode $ M.fromList [("recommended" :: T.Text, recommended) ]


toFaveRecord :: FaveRecordRequest -> FaveRecord
toFaveRecord frr = (favedBy frr, rating frr, photographer frr, photo frr)

toFollowRecord :: FollowRecordRequest -> FollowRecord
toFollowRecord frr = (follower frr, followed frr)
