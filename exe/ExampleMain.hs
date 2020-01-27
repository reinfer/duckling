-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Applicative hiding (empty)
import Control.Arrow ((***))
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString, empty)
import qualified Data.ByteString as BS
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Data.Time.LocalTime.TimeZone.Series
import GHC.Generics
import Prelude
import System.Directory
import System.Timeout (timeout)
import TextShow
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Snap.Core
import Snap.Http.Server

import Duckling.Core
import Duckling.Data.TimeZone
import Duckling.Resolve (DucklingTime)

microsPerSecond :: Int
microsPerSecond = 1000000

documentTimeoutMicros :: Int
documentTimeoutMicros = 5 * microsPerSecond

createIfMissing :: FilePath -> IO ()
createIfMissing f = do
  exists <- doesFileExist f
  unless exists $ writeFile f ""

setupLogs :: IO ()
setupLogs = do
  createDirectoryIfMissing False "log"
  createIfMissing "log/error.log"
  createIfMissing "log/access.log"

main :: IO ()
main = do
  setupLogs
  tzs <- loadTimeZoneSeries "/usr/share/zoneinfo/"
  quickHttpServe $
    route
      [ ("targets", method GET targetsHandler)
      , ("health-private", method GET healthPrivateHandler)
      , ("parse", method POST $ parseHandler tzs)
      ]

-- | Write with context length.
writeContent :: ByteString -> Snap ()
writeContent byteString = do
  modifyResponse $ setContentLength (fromIntegral $ BS.length byteString)
  writeBS byteString

writeLazyContent :: LBS.ByteString -> Snap ()
writeLazyContent lazy = writeContent $ LBS.toStrict lazy

-- | Health check
healthPrivateHandler :: Snap ()
healthPrivateHandler = writeContent "ok\n"

-- | Return which languages have which dimensions
targetsHandler :: Snap ()
targetsHandler = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLazyContent $ encode $
    HashMap.fromList . map dimText $ HashMap.toList supportedDimensions
  where
    dimText :: (Lang, [Some Dimension]) -> (Text, [Text])
    dimText = (Text.toLower . showt) *** map (\(This d) -> toName d)


-- | Parse some text into the given dimensions
--
type TimezoneHashMap = HashMap Text TimeZoneSeries

parseDocument :: Options -> Locale -> TimezoneHashMap -> [Some Dimension] -> HashSet Text -> ((Text, Integer), Text) -> [Entity]
parseDocument options thisLocale tzs parseDimensionList filterDimensionSet ((timezone, rawIntRef), text) =
    filter keepEntity $ parse text context options parseDimensionList
  where
    context = Context
      { referenceTime = refTime
      , Duckling.Core.locale = thisLocale
      }
    refTime = makeReftime tzs timezone $ posixSecondsToUTCTime $ fromInteger rawIntRef / 1000

    keepEntity entity = HashSet.member (dim entity) filterDimensionSet

data ParseRequest = ParseRequest {
      texts :: [Text]
    , referenceTimes :: [Integer]
    , language :: Maybe Text
    , parseDimensions  :: [Text]
    , filterDimensions  :: [Text]
    , timezones :: [Text]
    , locale :: Maybe Text
    , latent :: Maybe Bool
    } deriving (Generic, Show)

instance FromJSON ParseRequest

parseHandler :: TimezoneHashMap -> Snap ()
parseHandler tzs = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  body <- readRequestBody (8 * 1024 * 1024)
  case (decode body) of
    (Nothing) -> do
      modifyResponse $ setResponseStatus 400 "Bad Request"
      writeContent "{\"status\": \"error\", \"message\": \"Bad Request.\"}"
    (Just request) -> do
      let
        thisLocale = maybe (makeLocale (parseLang $ language request) Nothing) parseLocale (Main.locale request)
        options = Options {withLatent = fromMaybe False (Main.latent request)}
        parseDimensionList = mapMaybe fromName (parseDimensions request)
        filterDimensionSet = HashSet.fromList (filterDimensions request)
        configuredParse = parseDocument options thisLocale tzs parseDimensionList filterDimensionSet
        documents = (zip (zip (timezones request) (referenceTimes request)) (texts request))

        emptyEntitiesOnTimeout :: ((Text, Integer), Text) -> Maybe [Entity] -> Snap [Entity]
        emptyEntitiesOnTimeout (_, text) Nothing = do
          logError
            $ Text.encodeUtf8
            $ Text.concat [
              "Timed out when parsing entities in document: "
              , Text.pack (show text)
              , "."
            ]
          return []
        emptyEntitiesOnTimeout _ (Just entities) = return entities

        configuredParseWithTimeout document = do
          entities <- liftIO
            $ timeout documentTimeoutMicros
            $ return
            $! configuredParse document
          emptyEntitiesOnTimeout document entities


      _ <- setTimeout
        $ (documentTimeoutMicros * (length documents)) `quot` microsPerSecond
      parsedResult <- sequence $ map configuredParseWithTimeout documents
      writeLazyContent $ encode parsedResult
  where
    defaultLang = EN
    defaultLocale = makeLocale defaultLang Nothing

    parseLocale :: Text -> Locale
    parseLocale thisLocale = maybe defaultLocale (`makeLocale` mregion) mlang
      where
        (mlang, mregion) = case chunks of
          [a, b] -> (readMaybe a :: Maybe Lang, readMaybe b :: Maybe Region)
          _      -> (Nothing, Nothing)
        chunks = map Text.unpack . Text.split (== '_') . Text.toUpper $ thisLocale

    parseLang :: Maybe Text -> Lang
    parseLang l = fromMaybe defaultLang $ l >>=
      readMaybe . Text.unpack . Text.toUpper
