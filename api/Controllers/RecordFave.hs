{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Controllers.RecordFave(handleRecordFave, handleRecordFollow) where

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

handleRecordFave :: Value -> [T.Text] -> IO Response
handleRecordFave value _ = do
        let (Success frr) = fromJSON value
        relationship <- recordFave $ toFaveRecord frr
        return $ responseLBS
             status200
             [("Content-Type", "application/json")]
             $ encode $ M.fromList [("status" :: T.Text, "success":: T.Text)]

handleRecordFollow :: Value -> [T.Text] -> IO Response
handleRecordFollow value _ = do
        let (Success frr) = fromJSON value
        relationship <- recordFollow $ toFollowRecord frr
        return $ responseLBS
             status200
             [("Content-Type", "application/json")]
             $ encode $ M.fromList [("status" :: T.Text, "success":: T.Text)]

toFaveRecord :: FaveRecordRequest -> FaveRecord
toFaveRecord frr = (favedBy frr, rating frr, photographer frr, photo frr)

toFollowRecord :: FollowRecordRequest -> FollowRecord
toFollowRecord frr = (follower frr, followed frr)
