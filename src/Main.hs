{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           System.Environment
import qualified Data.Text as T
import           Text.Read (readMaybe)
import           Network.Wreq
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.ByteString.Lazy as B
import           Control.Monad
import           Data.Maybe (fromMaybe)
import           GHC.Generics

type ApiResponse = Response BS.ByteString
type ZIPCode = String

data AppConfig = AppConfig
  { dsKey :: T.Text
  , dsUrl :: T.Text
  , mapKey :: T.Text
  , mapUrl :: T.Text
  } deriving (Show, Generic)

instance FromJSON AppConfig
instance ToJSON AppConfig

data Location = Location
  { latitude :: Double
  , longitude :: Double
  } deriving (Eq, Show)

makeDsUrl :: AppConfig -> Location -> String
makeDsUrl ds loc = strUrl ++ strKey ++ "/" ++ strLoc ++ "?units=si"
 where
  strUrl = T.unpack $ dsUrl ds
  strKey = T.unpack $ dsKey ds
  strLoc = show (latitude loc) ++ "," ++ show (longitude loc)

makeMapUrl :: AppConfig -> ZIPCode -> String
makeMapUrl conf zipc = strUrl ++ strKey ++ "&location=" ++ zipc ++ "+usa"
 where
  strUrl = T.unpack $ mapUrl conf
  strKey = T.unpack $ mapKey conf

getLocation :: ApiResponse -> Maybe Location
-- Using applicative syntax since I have two Maybes for lat and lng
getLocation r = Location <$> lat <*> lng
 where
  lat =
    r
      ^? responseBody
      .  key "results"
      .  nth 0
      .  key "locations"
      .  nth 0
      .  key "latLng"
      .  key "lat"
      .  _Double
  lng =
    r
      ^? responseBody
      .  key "results"
      .  nth 0
      .  key "locations"
      .  nth 0
      .  key "latLng"
      .  key "lng"
      .  _Double

getCurrTemp :: ApiResponse -> Maybe Double
getCurrTemp r =
  r ^? responseBody . key "currently" . key "temperature" . _Double
  -- Roughly equivalent to r.responseBody.currently.temperature in JS
  -- but returns value in a Maybe to denote possible failure

formatTemp :: Double -> String
formatTemp x = show x ++ " °C"

getHead :: [a] -> Maybe a
getHead []    = Nothing
getHead (x:_) = Just x

-- Abstracting around this because I hate code repetition.
makeRequest
  :: (Applicative m, Traversable m)
  => m AppConfig
  -> (AppConfig -> a -> String)
  -> m a
  -> IO (m ApiResponse)
makeRequest conf f x = sequenceA $ get <$> (f <$> conf <*> x)

-- todo: config should be in a Reader
main :: IO ()
main = do
  args    <- getArgs
  config  <- decode <$> B.readFile "config.json" :: IO (Maybe AppConfig)
  locResp <- makeRequest config makeMapUrl (getHead args)
  r       <- makeRequest config makeDsUrl (locResp >>= getLocation)
  putStrLn $ maybe "Error while retrieving weather data"
                   formatTemp
                   (r >>= getCurrTemp)
