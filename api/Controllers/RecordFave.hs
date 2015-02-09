{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Controllers.RecordFave(handleRecordFave) where

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

$(deriveJSON defaultOptions ''FaveRecordRequest)

handleRecordFave :: Value -> [T.Text] -> IO Response
handleRecordFave value _ = do
        let (Success frr) = fromJSON value
        relationship <- recordFave $ toFaveRecord frr
        return $ responseLBS
             status200
             [("Content-Type", "application/json")]
             $ encode $ M.fromList [("status" :: T.Text, "success":: T.Text)]

toFaveRecord :: FaveRecordRequest -> FaveRecord
toFaveRecord frr = (favedBy frr, rating frr, photographer frr, photo frr)
