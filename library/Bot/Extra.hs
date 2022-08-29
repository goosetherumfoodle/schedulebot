{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fwarn-unused-do-bind #-}

module Bot.Example where

import Prelude hiding (until)

import Bot.Twilio (Contact(..))

import Data.Char (ord)
import Control.Monad.STM (atomically)
import Test.QuickCheck (generate, elements)
import Control.Monad (join)
import Data.Foldable (asum)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.RawString.QQ (r)
import Data.List (foldl', find)
import qualified Network.URI.Encode as URI
import Xmlbf (ToXml(..), element, text)
import Web.HttpApiData -- (ToHttpApiData(..))
import qualified Data.HashMap.Strict as HMap
import Servant.XML
import Web.Internal.FormUrlEncoded (Form, unForm, FromForm(..))
import Network.Wai (requestHeaders)
import Network.Wai.Handler.Warp (setLogger, setPort, runSettings, defaultSettings)
import Network.Wai.Logger (withStdoutLogger)
import Prettyprinter.Render.Text (renderStrict)
import Time.System (localDateCurrentAt)
import qualified Data.Map as Map
import Formatting (format, (%))
import qualified Formatting.Formatters as FMT
import Formatting.Formatters (left, int)
import Prettyprinter (Pretty(..), defaultLayoutOptions, layoutPretty, vsep, (<+>))
import Data.Maybe (catMaybes)
import Data.Int (Int64)
import Data.Yaml (decodeFileThrow, encodeFile)
import Control.Monad.IO.Class (liftIO)
import Twilio (runTwilio')
import Twilio.Messages (PostMessage(..))
import qualified Twilio.Messages as TWIL
import Data.Map (Map)
import Text.Read (readMaybe)
import qualified Data.Hourglass as HG
import Data.Text (Text, toLower, strip, pack, replace, unpack)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Data.Text.Encoding (encodeUtf8)
import Network.Google.OAuth2.JWT (SignedJWT, fromPEMString, getSignedJWT)
import Control.Lens (Identity, preview, (?~), (&), (^?), (.~))
import Data.Aeson.Types (Parser)
import Data.Aeson.Lens (key)
import Control.Exception (Exception, throwIO)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Vector ((!), (!?))
import System.Environment (getEnv)
import Text.Parsec ( ParseError
                   , ParsecT
                   , many
                   , anyChar
                   , manyTill
                   , parse
                   , try
                   , count
                   , digit
                   , char
                   , spaces
                   , string
                   , (<|>))
import Network.Wreq ( FormParam(..)
                    , postWith
                    , param
                    , post
                    , responseBody
                    , getWith
                    , oauth2Bearer
                    , auth
                    , defaults)
import Data.Aeson (FromJSON(..)
                  , ToJSON(..)
                  , Result(..)
                  , Value(..)
                  , fromJSON
                  , withObject
                  , withText
                  , withArray
                  , object
                  , pairs
                  , encode
                  , decode
                  , (.:)
                  , (.:?)
                  , (.=))
import Data.Hourglass ( UTC(..)
                      , Timeable
                      , LocalTime
                      , DateTime(..)
                      , WeekDay(..)
                      , ISO8601_DateAndTime(..)
                      , Date(..)
                      , TimeOfDay(..)
                      , Month(..)
                      , Hours(..)
                      , Minutes(..)
                      , TimezoneOffset(..)
                      , timezoneOffset
                      , dateAddPeriod
                      , timeGetDateTimeOfDay
                      , getWeekDay
                      , timeDiff
                      , toSeconds
                      , fromSeconds
                      , timeGetDate
                      )

newtype DisplayMonthDate a = DisplayMonthDate a
newtype Concise a = Concise a
newtype JWTException = JWTException String




-- instance Bitraversable ShiftTime where

-- instance FromHttpApiData Cookies where
--    parseHeader = Right . parseCookies










testAuthToken :: IO ByteString
testAuthToken = loadEnv >> googleJWT >>= getAuthToken


contains :: ParsecT Text () Identity [String] -> Text -> Bool
contains p input = toBool $ parse p "contains" (toLower input)
  where
    toBool = either (const False) (const True)

nameParser :: Text -> ParsecT Text u Identity [String]
nameParser name = traverse (\part -> manyTill anyChar $ string part) nameParts
  where
    nameParts = words . unpack . toLower $ name

stafferOnCal :: Foldable f => Text -> f (GCalEvent a) -> Bool
stafferOnCal staffer = any (maybe False hasName . gCalSummary)
  where
    hasName = contains $ nameParser staffer




-- -- for fun
-- getTodayStub :: IO (LocalTime Date)
-- getTodayStub = pure $ HG.localTime localOffset date
--   where
--     date = Date { dateDay = 26, dateMonth = October, dateYear = 2018 }


-- TODO: save these somewhere for calculations
-- eventsInRange :: StartTime -> EndTime -> IO [GCalEventI]
-- eventsInRange start end = do
--   rawEvents <- getEvents (start, end)
--   events <- validateGCalEvent' `traverse` rawEvents
--   pure events

-- summaryDurationsInRange :: StartTime -> EndTime -> IO [(Text, Hours, Minutes)]
-- summaryDurationsInRange s e = eventBySummDuration <$> eventsInRange s e



eventDuration :: GCalEventI -> (Minutes, HG.Seconds)
eventDuration a = fromSeconds $ timeDiff (inUTC . gCalEnd $ a) (inUTC . gCalStart $ a)
    where
    inUTC = HG.localTimeUnwrap . runIntern

totalDuration :: Foldable f => f GCalEventI -> (Minutes, HG.Seconds)
totalDuration = foldr sumMS (0,0)
  where
    sumMS :: GCalEventI -> (Minutes, HG.Seconds) -> (Minutes, HG.Seconds)
    sumMS e t = ((fst (eventDuration e) + fst t), (snd (eventDuration e) + snd t))

-- used to aggregate staffer hours
eventBySummDuration :: Foldable f => f GCalEventI -> [(Text, Hours, Minutes)]
eventBySummDuration  =
  fmap reshapeTime
  . Map.toList
  . foldr bySummary Map.empty
  where
    bySummary :: GCalEvent InternTime -> Map Text (Minutes, HG.Seconds) -> Map Text (Minutes, HG.Seconds)
    bySummary e  =
      Map.insertWith
      sumMS
      (maybe " " (toLower . strip) . gCalSummary $ e)
      $ eventDuration e

    reshapeTime :: (Text, (Minutes, HG.Seconds)) -> (Text, Hours, Minutes)
    reshapeTime (summ, (m, s)) =
      (summ, (fst $ toHours m s), (snd $ toHours m s))

    sumMS :: (Minutes, HG.Seconds) -> (Minutes, HG.Seconds) -> (Minutes, HG.Seconds)
    sumMS (m1, s1) (m2, s2) = (m1 + m2, s1 + s2)

toHours :: Minutes -> HG.Seconds -> (Hours, Minutes)
toHours m s =
  let
    totalSeconds = (toSeconds m) + s
    (hours, remSecs) = fromSeconds totalSeconds :: (Hours, HG.Seconds)
    (mins, _) = fromSeconds remSecs :: (Minutes, HG.Seconds)
  in
    (hours, mins)

addAvailableshiftsCookie' :: ToJSON (Gaps [a]) => Gaps [a] -> TwilioMsgResponse -> Headers '[Header "Set-Cookie" SetCookie] TwilioMsgResponse
addAvailableshiftsCookie' (Gaps []) = destroyCookie
addAvailableshiftsCookie' gaps = do
  let cookie = emptyAvailableShiftsCookie {
        setCookieValue = URI.encodeByteString . BSL.toStrict . encode $ gaps
        }
  addHeader cookie
