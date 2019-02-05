-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Applicative hiding (empty)
import Control.Arrow ((***))
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString, empty)
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Data.Time.LocalTime.TimeZone.Series
import GHC.Generics
import Prelude
import System.Directory
import System.Environment (lookupEnv)
import TextShow
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Snap.Core
import Snap.Http.Server

import Duckling.Core
import Duckling.Data.TimeZone
import Duckling.Resolve (DucklingTime)

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

-- | Health check
healthPrivateHandler :: Snap ()
healthPrivateHandler = writeBS "ok\n"

-- | Return which languages have which dimensions
targetsHandler :: Snap ()
targetsHandler = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS $ encode $
    HashMap.fromList . map dimText $ HashMap.toList supportedDimensions
  where
    dimText :: (Lang, [Some Dimension]) -> (Text, [Text])
    dimText = (Text.toLower . showt) *** map (\(This d) -> toName d)


-- | Parse some text into the given dimensions
--
type TimezoneHashMap = HashMap Text TimeZoneSeries

parseDocument :: Options -> Locale -> TimezoneHashMap -> [Some Dimension] -> ((Text, Integer), Text) -> [Entity]
parseDocument options thisLocale tzs dims ((timezone, rawIntRef), text) =
    parse text context options dims
  where
    context = Context
      { referenceTime = refTime
      , Duckling.Core.locale = thisLocale
      }
    refTime = makeReftime tzs timezone $ posixSecondsToUTCTime $ fromInteger rawIntRef / 1000

data ParseRequest = ParseRequest {
      texts :: [Text]
    , referenceTimes :: [Integer]
    , language :: Maybe Text
    , dimensions  :: [Text]
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
      writeBS "{\"status\": \"error\", \"message\": \"Bad Request.\"}"
    (Just request) -> do
      let
        thisLocale = maybe (makeLocale (parseLang $ language request) Nothing) parseLocale (Main.locale request)
        options = Options {withLatent = fromMaybe False (Main.latent request)}
        dims = mapMaybe fromName (dimensions request)

        parsedResult = map
          (parseDocument options thisLocale tzs dims)
          (zip (zip (timezones request) (referenceTimes request)) (texts request))

      writeLBS $ encode parsedResult
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
