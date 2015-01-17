{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

import           Control.Exception        (SomeException)
import           Control.Exception.Lifted (handle)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (Value, encode, object, (.=), FromJSON, ToJSON)
import           Data.Aeson.Parser        (json)
import           Data.Aeson.Types
import           Data.Aeson.TH            (deriveJSON, defaultOptions)
import           Data.ByteString          (ByteString)
import           Data.Conduit             (($$))
import           Data.Conduit.Attoparsec  (sinkParser)
import           Network.HTTP.Types       (status200, status400)
import           Network.Wai              (Application, Response, responseLBS)
import           Network.Wai.Conduit      (sourceRequestBody)
import           Network.Wai.Handler.Warp (run)


data Photographer =
  Photographer {
    flickrId :: String,
    name :: String
   } deriving (Show)

$(deriveJSON defaultOptions ''Photographer)

main :: IO ()
main = run 3000 app

app :: Application
app req sendResponse = handle (sendResponse . invalidJson) $ do
    value <- sourceRequestBody req $$ sinkParser (fmap fromJSON json)
    newValue <- liftIO $ modValue value
    sendResponse $ responseLBS
        status200
        [("Content-Type", "application/json")]
        $ encode newValue

invalidJson :: SomeException -> Response
invalidJson ex = responseLBS
    status400
    [("Content-Type", "application/json")]
    $ encode $ object
        [ ("message" .= show ex)
        ]

-- Application-specific logic would go here.
modValue :: Result Photographer -> IO Photographer
modValue (Success p) = return $ Photographer "fd" "dfd"
