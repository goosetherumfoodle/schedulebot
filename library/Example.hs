{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

--import Debug.Trace

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (groupBy)
import Text.Read (readMaybe)
import Data.Hourglass ( DateTime(..)
                      , WeekDay(..)
                      , ISO8601_DateAndTime(..)
                      , Date(..)
                      , TimeOfDay
                      , Elapsed
                      , dateAddPeriod
                      , timeGetDateTimeOfDay
                      , getWeekDay
                      , timeGetDateTimeOfDay
                      , dtDate
                      , timeGetElapsed
                      , timeParse
                      )
import qualified Data.Hourglass as HG
import Data.Text (Text, pack, replace, unpack)
import Data.Text.Encoding (encodeUtf8)
import Network.Google.OAuth2.JWT (SignedJWT, fromPEMString, getSignedJWT)
import Control.Lens (Identity, preview, (?~), (&), (^?))
import Data.Aeson.Types (Parser)
import Data.Aeson.Lens (key)
import Control.Exception (throwIO, Exception)
import Data.ByteString.Char8 (ByteString)
import Data.Vector ((!), (!?))
import LoadEnv (loadEnv)
import System.Environment (getEnv)
import Text.Parsec (parse, try, count, digit, char, (<|>), ParseError, ParsecT)
import Network.Wreq (FormParam(..)
                    , post
                    , responseBody
                    , getWith
                    , oauth2Bearer
                    , auth
                    , defaults)
import Data.Aeson (FromJSON(..)
                  , Result(..)
                  , fromJSON
                  , withObject
                  , withText
                  , withArray
                  , (.:)
                  , (.:?))

newtype JWTException = JWTException String

instance Exception JWTException

instance Show JWTException where
  show (JWTException str) = "Error creating JWT: " ++ show str

getAcctEmail :: IO Text
getAcctEmail = getEnv acctVar >>= pure . pack
  where acctVar = "GOOGLE_SERVICE_ACCT_EMAIL"

getPEM :: IO String
getPEM = do
  rawPem <- getEnv keyVar
  -- getEnv adds an extra escape to the newlines, so we remove those
  let pem = unpack $ replace "\\n" "\n" $ pack rawPem
  pure $ pem
  where keyVar = "GOOGLE_ACCT_KEY"

googleJWT :: IO SignedJWT
googleJWT = do
  email <- getAcctEmail
  pem <- fromPEMString =<< getPEM
  jwt <- getSignedJWT email Nothing [calAuth] Nothing pem
  either (throwIO . JWTException) pure jwt
  where
    calAuth = "https://www.googleapis.com/auth/calendar"

getAuthToken :: SignedJWT -> IO ByteString
getAuthToken jwt = do
  resp <- post "https://www.googleapis.com/oauth2/v4/token"
    [
      "grant_type" := grantString
    , "assertion" := show jwt
    ]
  mToken <- pure $ unwrapResult $ fmap fromJSON $ resp ^? responseBody . key "access_token"
  maybe (throwIO GAuthTokenException) pure $ mToken
  where
    grantString = "urn:ietf:params:oauth:grant-type:jwt-bearer" :: Text

unwrapResult :: Maybe (Result Text) -> Maybe ByteString
unwrapResult (Just (Success txt)) = Just . encodeUtf8 $ txt
unwrapResult _ = Nothing

testAuthToken :: IO ByteString
testAuthToken = loadEnv >> googleJWT >>= getAuthToken

data GAuthTokenException = GAuthTokenException

instance Exception GAuthTokenException

instance Show GAuthTokenException where
  show GAuthTokenException = "Error fetching google auth token"

unwrapEvents :: Maybe (Result [RawGCalEvent]) -> IO [RawGCalEvent]
unwrapEvents (Just (Success events)) = pure events
unwrapEvents _ = throwIO ResponseErrorGCalEvents

getEvents :: IO [RawGCalEvent]
getEvents = do
  loadEnv
  token <- getAuthToken =<< googleJWT
  resp <- getWith (defaults & auth ?~ oauth2Bearer token) url
  let mEvents = fromJSON <$> preview (responseBody . key "items") resp
  unwrapEvents mEvents
  where calId = "1j5jbe646s86vm99f15ul91eac@group.calendar.google.com"
        url = "https://www.googleapis.com/calendar/v3/calendars/"
          ++ calId
          ++ "/events"

data ResponseErrorGCalEvents = ResponseErrorGCalEvents

instance Exception ResponseErrorGCalEvents

instance Show ResponseErrorGCalEvents where
  show ResponseErrorGCalEvents =
    "Error: No events found in google calendar response body"

type RawGCalEvent = GCalEvent (ISO8601 String)
type GCalEventE = GCalEvent Elapsed

newtype ISO8601 a = ISO8601 a deriving (Show, Eq)

-- todo: refactor to use Period
data GCalEvent a = GCalEvent
  { gCalSummary :: String
  , gCalDesc :: (Maybe String)
  , gCalStart :: a
  , gCalEnd :: a
  } deriving (Show, Eq)

instance FromJSON (ISO8601 String) where
  parseJSON = withObject "GCalDateTime" $ \v -> ISO8601 <$> v .: "dateTime"

instance FromJSON (GCalEvent (ISO8601 String)) where
  parseJSON = withObject "GCalEvent" $ \o ->
    GCalEvent
      <$> o .: "summary"
      <*> o .:? "description"
      <*> o .: "start"
      <*> o .: "end"

validateGCalEvent :: RawGCalEvent -> Either ISO8601Error GCalEventE
validateGCalEvent (GCalEvent smry dsc end st) = GCalEvent <$> pure smry <*> pure dsc <*> (parse8601 end)  <*> (parse8601 st)

-- I'm assuming that this correctly handles the timezone
parse8601 :: ISO8601 String -> Either ISO8601Error Elapsed
parse8601 (ISO8601 s) = maybe (Left $ ISO8601Error s) Right $ timeGetElapsed <$> timeParse ISO8601_DateAndTime s

data ISO8601Error = ISO8601Error String

instance Exception ISO8601Error

instance Show ISO8601Error where
  show (ISO8601Error str) = "Error: Couldn't parse ISO8601 date format from: " ++ str

-- todo: update these
type RawShiftTime = ShiftTime (Int, Int)
type ShiftTimeOfDay = ShiftTime TimeOfDay

newtype ShiftTime a = ShiftTime { runShiftTime :: a } deriving (Show, Eq)

type ShiftWeekTime = ShiftWeek ShiftTimeOfDay
type ShiftWeekRaw = ShiftWeek RawShiftTime

-- todo: validate times
-- validateShiftWeek :: ShiftWeekRaw -> ShiftWeekHM
-- validateShiftWeek


data Period a = Period { periodStart :: a
                       , periodEnd :: a
                       , periodName :: Maybe String
                       } deriving (Show, Eq)

data ShiftWeek a = ShiftW { wkMon :: [Period a]
                          , wkTue :: [Period a]
                          , wkWed :: [Period a]
                          , wkThu :: [Period a]
                          , wkFri :: [Period a]
                          , wkSat :: [Period a]
                          , wkSun :: [Period a]
                          } deriving (Show, Eq)

clockTimeParser :: ParsecT Text u Identity (String, String)
clockTimeParser = do
  h <- (try $ count 2 digit) <|> count 1 digit
  _ <- char ':'
  m <- count 2 digit
  pure $ (h, m)

instance FromJSON RawShiftTime where
  parseJSON = withText "RawShiftTime" $ \str -> do
    (hrs, mins) <- errHandler $ parse clockTimeParser "timeParser" str
    ShiftTime <$> ((,) <$> readHours hrs <*> readMinutes mins)
      where errHandler :: Either ParseError (String, String) -> Parser (String, String)
            errHandler (Right a) = pure a
            errHandler (Left err) = fail $ "Error parsing hours: " ++ show err

            readHours :: Monad m => String -> m Int
            readHours hrs | Nothing <- readMaybe hrs  :: Maybe Int  = fail $ "Couldn't parse hours"
                          | Just hrsParsed <- readMaybe hrs = pure hrsParsed

            readMinutes :: Monad m => String -> m Int
            readMinutes mins | Nothing <- readMaybe mins :: Maybe Int = fail $ "Couldn't parse minutes"
                             | Just minsParsed <- readMaybe mins = pure minsParsed

instance FromJSON a => FromJSON (Period a) where
  parseJSON = withArray "Period" $ \ary ->
    Period
    <$> (parseJSON $ ary ! 0)
    <*> (parseJSON $ ary ! 1)
    <*> (sequence $ parseJSON <$> ary !? 2)

instance FromJSON a => FromJSON (ShiftWeek a) where
  parseJSON = withObject "ShiftWeek" $ \o ->
    ShiftW
      <$> o .: "monday"
      <*> o .: "tuesday"
      <*> o .: "wednesday"
      <*> o .: "thursday"
      <*> o .: "friday"
      <*> o .: "saturday"
      <*> o .: "sunday"

newtype Gaps a = Gaps a deriving (Show, Eq)

getGapsInRange :: ShiftWeekTime -> (Date, Date) -> [GCalEventE] -> Gaps [Period Elapsed]
getGapsInRange shifts dateRange events =
  Gaps $ concat $ fmap (uncurry dayGetGaps) shiftsAndEvents
  where
    shiftsAndEvents = pairShiftsAndEvents datedShifts datedEvents
    datedShifts = shiftsInRange dateRange shifts
    datedEvents = Map.fromList $ groupByDate events

dayGetGaps :: [Period Elapsed] -> [GCalEventE] -> [Period Elapsed]
dayGetGaps shifts events = foldr (flip alterGaps) shifts events

pairShiftsAndEvents :: [(Date, [Period Elapsed])] -> Map Date [GCalEventE] -> [([Period Elapsed], [GCalEventE])]
pairShiftsAndEvents [] _ = []
pairShiftsAndEvents (d:ds) e
  | Just events <- Map.lookup (fst d) e = (snd d, events) : pairShiftsAndEvents ds e
  | Nothing     <- Map.lookup (fst d) e = (snd d, []) : pairShiftsAndEvents ds e

shiftsInRange :: (Date, Date) -> ShiftWeekTime -> [(Date, [Period Elapsed])]
shiftsInRange dateRange shifts = fmap (datesShifts shifts) shiftDates
  where
    shiftDates :: [Date]
    shiftDates = uncurry datesFromTo dateRange

    datesShifts :: ShiftWeekTime -> Date -> (Date, [Period Elapsed])
    datesShifts s date = (,) date
      $ periodTimeToElapsed date
      <$> pickShiftSelector (getWeekDay date) s

datesFromTo :: Date -> Date -> [Date]
datesFromTo d1 d2 | d1 < d2   = d1 : datesFromTo (nextDate d1) d2
                  | otherwise = [d2]

nextDate :: Date -> Date
nextDate = flip dateAddPeriod day
  where
    day = HG.Period
      { HG.periodYears = 0
      , HG.periodMonths = 0
      , HG.periodDays = 1
      }

groupByDate :: [GCalEventE] -> [(Date, [GCalEventE])]
groupByDate = fmap addDate . groupBy (\a b -> eventDate a == eventDate b)
  where
    addDate :: [GCalEvent Elapsed] -> (Date, [GCalEvent Elapsed])
    addDate events = ((eventDate . head $ events), events)
    eventDate = dtDate . timeGetDateTimeOfDay . gCalStart

periodTimeToElapsed :: Date -> Period ShiftTimeOfDay -> Period Elapsed
periodTimeToElapsed date a =
  Period
  (getElapsed . periodStart $ a)
  (getElapsed . periodEnd $ a)
  (periodName a)
  where
    getElapsed = timeGetElapsed . getDate . runShiftTime
    getDate = DateTime date

alterGaps :: [Period Elapsed] -> GCalEventE -> [Period Elapsed]
alterGaps [] _ = []
alterGaps gaps@(g:gs) event | PostGap <- gapRel g event = gaps
                            | PriorGap <- gapRel g event = g : alterGaps gs event
                            | SubsetGap <- gapRel g event = alterGaps gs event
                            | StrictSupersetGap <- gapRel g event = splitGap g event ++ alterGaps gs event
                            | IntersectGap <- gapRel g event = shrinkGap g event : alterGaps gs event

data GapRelationship = PostGap | PriorGap | SubsetGap | StrictSupersetGap | IntersectGap deriving (Show, Eq)

splitGap :: Period Elapsed -> GCalEventE -> [Period Elapsed]
splitGap g e = [
    Period (periodStart g) (gCalStart e) Nothing
  , Period (gCalEnd e) (periodEnd g) Nothing
  ]

shrinkGap :: Period Elapsed -> GCalEventE -> Period Elapsed
shrinkGap g e | periodStart g < gCalStart e  = Period (periodStart g) (gCalStart e) Nothing
              | otherwise                    = Period (gCalEnd e) (periodEnd g) Nothing

gapRel :: Period Elapsed -> GCalEventE -> GapRelationship
gapRel g e | periodEnd g <= gCalStart e = PriorGap
           | periodStart g >= gCalEnd e = PostGap
           | periodStart g >= gCalStart e && periodEnd g <= gCalEnd e = SubsetGap
           | periodStart g < gCalStart e && periodEnd g > gCalEnd e = StrictSupersetGap
           | otherwise = IntersectGap

pickShiftSelector :: WeekDay -> (ShiftWeek a -> [Period a])
pickShiftSelector Monday = wkMon
pickShiftSelector Tuesday = wkTue
pickShiftSelector Wednesday = wkWed
pickShiftSelector Thursday = wkThu
pickShiftSelector Friday = wkFri
pickShiftSelector Saturday = wkSat
pickShiftSelector Sunday = wkSun
