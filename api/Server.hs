{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception        (SomeException)
import           Control.Exception.Lifted (handle)
import           Control.Monad.IO.Class   (liftIO)
import           Controllers.RecordFave
import           Data.Aeson               (Value, encode, object, (.=), FromJSON, ToJSON)
import           Data.Aeson.Parser        (json)
import           Data.Aeson.Types
import qualified Data.HashMap.Lazy as M

import           Data.Conduit             (($$))
import           Data.Conduit.Attoparsec  (sinkParser)
import           Network.HTTP.Types       (status200, status400)
import           Network.Wai
import           Network.Wai.Conduit      (sourceRequestBody)
import           Network.Wai.Handler.Warp (run)
import qualified Data.Text as T


main :: IO ()
main = run 3000 app

app :: Application
app req sendResponse = handle (sendResponse . invalidJson) $ do
    value <- sourceRequestBody req $$ sinkParser json
    let (m,p) = (requestMethod req, pathInfo req)
    let handler = case (m, p) of
                    ("POST", ["faves"])   -> postFaveRecord
                    ("POST", ["follows"]) -> postFollowRecord
                    ("GET", (_ : "recommendations" : _)) -> getRecommendation
    respContent <- liftIO $ handler value p
    sendResponse $ responseLBS
                      status200
                      [("Content-Type", "application/json")]
                      respContent


invalidJson :: SomeException -> Response
invalidJson ex = responseLBS
    status400
    [("Content-Type", "application/json")]
    $ encode $ object
        [ ("message" .= show ex)
        ]
