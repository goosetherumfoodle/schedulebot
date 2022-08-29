{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fwarn-unused-do-bind #-}

module Bot.Cal where

import Bot.Time
  ( Covered (..),
    Date (..),
    Days (..),
    End (..),
    EndDate,
    EndTime,
    Gaps (..),
    ISO8601 (..),
    ISO8601Error (..),
    InternTime (..),
    Minutes,
    Period (..),
    PeriodIntern (..),
    RawShiftTime,
    ShiftTimeOfDay,
    ShiftWeek (..),
    ShiftWeekRaw,
    ShiftWeekTime,
    Start (..),
    StartDate,
    StartTime,
    TimeOfDay,
    TimezoneOffset (..),
    -- , WeekDay(..)

    datesFromTo,
    internToDate,
    parse8601,
    periodDuration,
    periodTimeToElapsed,
    runIntern,
    show8601UTC,
    validateShiftWeek,
  )
import Bot.Twilio (configDir)
import Control.Exception (Exception, throwIO)
import Control.Lens
  ( Identity,
    contains,
    preview,
    (&),
    (.~),
    (?~),
    (^?),
  )
import Data.Aeson
  ( FromJSON (..),
    Result (..),
    ToJSON (..),
    Value (..),
    decode,
    encode,
    fromJSON,
    object,
    pairs,
    withArray,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.Lens (key)
import Data.ByteString.Char8 (ByteString)
import Data.Foldable (asum)
import qualified Data.Hourglass as HG
import Data.Int (Int64)
import Data.List (groupBy)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, replace, strip, toLower, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeFileThrow, encodeFile)
import LoadEnv (loadEnv)
import Network.Google.OAuth2.JWT (SignedJWT, fromPEMString, getSignedJWT)
import Network.Wreq
  ( FormParam (..),
    auth,
    defaults,
    getWith,
    oauth2Bearer,
    param,
    post,
    postWith,
    responseBody,
  )
import System.Environment (getEnv)
import Text.Parsec
  ( ParseError,
    ParsecT,
    anyChar,
    char,
    count,
    digit,
    many,
    manyTill,
    parse,
    spaces,
    string,
    try,
    (<|>),
  )

newtype JWTException = JWTException String

-- todo: refactor to use Period
data GCalEvent a = GCalEvent
  { gCalSummary :: Maybe Text
  , gCalDesc :: Maybe Text
  , gCalStart :: a
  , gCalEnd :: a
  } deriving (Show, Eq, Functor)

newtype StoreEvent a = StoreEvent { runStoreEvent :: a } deriving (Functor, Foldable, Traversable, Show)

newtype CalId a = CalId a

type RawGCalEvent = GCalEvent (ISO8601 Text)
data ResponseErrorGCalEvents = ResponseErrorGCalEvents String
type GCalEventI = GCalEvent InternTime

data GapRelationship =
    PostGap
  | PriorGap
  | SubsetGap
  | StrictSupersetGap
  | IntersectGap
  deriving (Show, Eq)

data GAuthTokenException = GAuthTokenException

mkGCalEvent :: PeriodIntern -> Text -> GCalEventI
mkGCalEvent p summ = GCalEvent (Just summ) botText (periodStart p) (periodEnd p)
  where
    botText = Just "Event created by shift bot."

getMinGapsInRange :: TimezoneOffset -> Minutes -> ShiftWeekTime -> (StartTime, EndTime) -> [GCalEventI] -> Gaps [Period InternTime]
getMinGapsInRange tzo minGap shifts range events =
  filter greaterThanMin <$> getAllGapsInRange tzo shifts range events
  where
    greaterThanMin = (>= minGap) . fst . periodDuration

-- TODO: should daterange be a period?
getAllGapsInRange :: TimezoneOffset -> ShiftWeekTime -> (StartTime, EndTime) -> [GCalEventI] -> Gaps [Period InternTime]
getAllGapsInRange tzo shifts (start, end) events =
  Gaps $ concat $ fmap (uncurry dayGetGaps) shiftsAndEvents
  where
    shiftsAndEvents = pairShiftsAndEvents datedShifts datedEvents
    datedShifts = shiftsInRange tzo (start, end) shifts
    datedEvents = Map.fromList $ groupByDate tzo events

-- TODO: should daterange be a period?
-- TODO: do we need tzo here? (It seems not as we're comparing intern to intern?)
getAllGapsAllDay :: TimezoneOffset -> [Covered GCalEventI] -> [StoreEvent GCalEventI] -> Gaps [Period InternTime]
getAllGapsAllDay _ coverage events =
  Gaps $ dayGetGaps storeEvents coveragePeriods
  where
    storeEvents = periodize . runStoreEvent <$> events
    coveragePeriods = runCovered <$> coverage

-- TODO: Generalize this logic, as we're now using it in more places than just
-- shifts (from a schedule) and events (meaning covered shifts)
-- maybe use some newtypes related to openings and coverage
dayGetGaps :: [Period InternTime] -> [GCalEventI] -> [Period InternTime]
dayGetGaps shifts events = foldr alterGaps shifts events

pairShiftsAndEvents :: [(Date, [PeriodIntern])] -> Map.Map Date [GCalEventI] -> [([PeriodIntern], [GCalEventI])]
pairShiftsAndEvents [] _ = []
pairShiftsAndEvents (d:ds) e =
  case Map.lookup (fst d) e of
    Just events -> (snd d, events) : pairShiftsAndEvents ds e
    Nothing     -> (snd d, []) : pairShiftsAndEvents ds e

alterGaps :: GCalEventI -> [PeriodIntern] -> [PeriodIntern]
alterGaps _ [] = []
alterGaps event gaps@(g:gs) =
  case gapRel g event of
    PostGap           -> gaps
    PriorGap          -> g : alterGaps event gs
    SubsetGap         -> alterGaps event gs
    StrictSupersetGap -> splitGap g event <> alterGaps event gs
    IntersectGap      -> shrinkGap g event : alterGaps event gs

splitGap :: PeriodIntern -> GCalEventI -> [PeriodIntern]
splitGap g e = [
    Period (periodStart g) (gCalStart e) Nothing
  , Period (gCalEnd e) (periodEnd g) Nothing
  ]

shrinkGap :: PeriodIntern -> GCalEventI -> PeriodIntern
shrinkGap g e | periodStart g < gCalStart e = Period (periodStart g) (gCalStart e) Nothing
              | otherwise                   = Period (gCalEnd e) (periodEnd g) Nothing
-- any shift that *starts* in range
shiftsInRange :: TimezoneOffset -> (StartTime, EndTime) -> ShiftWeekTime -> [(Date, [PeriodIntern])]
shiftsInRange tzo (start, end) shifts = filter hasShifts allDatesInRange
  where
    hasShifts :: (Date, [PeriodIntern]) -> Bool
    hasShifts = not . null . snd

    allDatesInRange :: [(Date, [PeriodIntern])]
    allDatesInRange = (fmap.fmap) (filter startsInRange) allDateShifts

    startsInRange :: PeriodIntern -> Bool
    startsInRange p =
         periodStart p >= runStart start
      && periodStart p <= runEnd end

    allDateShifts :: [(Date, [PeriodIntern])]
    allDateShifts = fmap (datesShifts shifts) shiftDates

    shiftDates :: [Date]
    shiftDates =
      uncurry datesFromTo (internToDate tzo <$> start, internToDate tzo <$> end)

    datesShifts :: ShiftWeekTime -> Date -> (Date, [PeriodIntern])
    datesShifts s date = (,) date
      $ periodTimeToElapsed date
      <$> pickShiftSelector (HG.getWeekDay date) s

gapRel :: PeriodIntern -> GCalEventI -> GapRelationship
gapRel g e
  | periodEnd g <= gCalStart e = PriorGap
  | periodStart g >= gCalEnd e = PostGap
  | periodStart g >= gCalStart e && periodEnd g <= gCalEnd e = SubsetGap
  | periodStart g < gCalStart e && periodEnd g > gCalEnd e = StrictSupersetGap
  | otherwise = IntersectGap

pickShiftSelector :: HG.WeekDay -> (ShiftWeek a -> [Period a])
pickShiftSelector HG.Monday    = wkMon
pickShiftSelector HG.Tuesday   = wkTue
pickShiftSelector HG.Wednesday = wkWed
pickShiftSelector HG.Thursday  = wkThu
pickShiftSelector HG.Friday    = wkFri
pickShiftSelector HG.Saturday  = wkSat
pickShiftSelector HG.Sunday    = wkSun

groupByDate :: TimezoneOffset -> [GCalEventI] -> [(Date, [GCalEventI])]
groupByDate tzo = fmap addDate . groupBy (\a b -> eventDate a == eventDate b)
  where
    addDate :: [GCalEventI] -> (Date, [GCalEventI])
    addDate events = ((eventDate . head $ events), events)
    eventDate = internToDate tzo . gCalStart

periodize :: GCalEventI -> PeriodIntern
periodize e = Period (gCalStart e) (gCalEnd e) Nothing

-- TODO: deal with paginated results (`nextPageToken`)
-- if we don't have the "singleEvents" query param set, then we might get
-- recurring dates before the earliest date we request
getEventsForCal :: CalId String -> (StartTime, EndTime) -> IO [RawGCalEvent]
getEventsForCal (CalId cid) ((Start start), (End end)) = do
  let (fmtMin, fmtMax) = (show8601UTC start, show8601UTC end)
  loadEnv
  token <- getAuthToken =<< googleJWT
  let reqOpts = defaults
             & auth ?~ oauth2Bearer token
             & param "orderBy" .~ ["startTime"]
             & param "singleEvents" .~ ["true"]
             & param "timeMin" .~ [fmtMin]
             & param "timeMax" .~ [fmtMax]
             & param "maxResults" .~ ["2500"]
  resp <- getWith reqOpts $ url cid
  let mEvents = fromJSON <$> preview (responseBody . key "items") resp
  catMaybes <$> (unwrapEvents mEvents)
  where url calID = "https://www.googleapis.com/calendar/v3/calendars/"
          <> calID
          <> "/events"

unwrapResult :: Maybe (Result Text) -> Maybe ByteString
unwrapResult (Just (Success txt)) = Just . encodeUtf8 $ txt
unwrapResult _ = Nothing

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

googleJWT :: IO SignedJWT
googleJWT = do
  email <- getAcctEmail
  pem <- fromPEMString =<< getPEM
  jwt <- getSignedJWT email Nothing [calAuth] Nothing pem
  either (throwIO . JWTException) pure jwt
  where
    calAuth = "https://www.googleapis.com/auth/calendar"

unwrapEvents :: Maybe (Result [Maybe RawGCalEvent]) -> IO [Maybe RawGCalEvent]
unwrapEvents (Just (Success events)) = pure events
unwrapEvents err = throwIO $ ResponseErrorGCalEvents $ show err

getAcctEmail :: IO Text
getAcctEmail = getEnv acctVar >>= pure . pack
  where acctVar = "GOOGLE_SERVICE_ACCT_EMAIL"


getPEM :: IO String
getPEM = do
  rawPem <- getEnv keyVar
  -- getEnv adds an extra escape to the newlines, so we remove those
  let pem = unpack $ replace "\\n" "\n" $ pack rawPem
  pure $ pem
  where
    keyVar = "GOOGLE_ACCT_KEY"

instance FromJSON a => FromJSON (GCalEvent a) where
  parseJSON = withObject "GCalEvent" $ \o ->
    GCalEvent
      <$> o .:? "summary"
      <*> o .:? "description"
      <*> o .: "start"
      <*> o .: "end"

instance Exception ResponseErrorGCalEvents

instance Show ResponseErrorGCalEvents where
  show (ResponseErrorGCalEvents a) =
    "Error: in google calandar response: " <> show a

instance Exception JWTException

instance Show JWTException where
  show (JWTException str) = "Error creating JWT: " <> show str

instance Exception GAuthTokenException

instance Show GAuthTokenException where
  show GAuthTokenException = "Error fetching google auth token"

-- TODO: Use timezone from cal to finish date?
-- or GADT to construct different type depending on date or datetime presence?
-- (can GADTs be used to parse (GCalEvent (ISO8601 Text)) OR (GCalEvent Date)?
instance FromJSON (ISO8601 Text) where
  parseJSON = withObject "GCalDateTime" $ \v -> do
     dt <- v .:? "dateTime"
     -- this is a hack to avoid solving the issue of how to handle parsing all-day events
     d <- (fmap.fmap) (<> "T00:00:00Z") (v .:? "date")
     case asum [dt, d] of
       Just date -> pure $ ISO8601 date
       Nothing -> fail "couldn't find \"dateTime\" or \"date\" in google calendar event"

validateGCalEvent :: RawGCalEvent -> Either ISO8601Error GCalEventI
validateGCalEvent (GCalEvent smry dsc end st) =
  GCalEvent
  <$> pure (toLower . strip <$> smry)
  <*> pure (toLower . strip <$> dsc)
  <*> (parse8601 end)
  <*> (parse8601 st)

validateGCalEvent' :: RawGCalEvent -> IO GCalEventI
validateGCalEvent' = either throwIO pure . validateGCalEvent

getMainLocCalId :: IO (CalId String)
getMainLocCalId = CalId <$> getEnv "MAIN_LOC_STAFFING_GCAL_ID"

hasOverlap :: [GCalEventI] -> GCalEventI -> Bool
hasOverlap onCal newEvent = ([eventPeriod] /=) $ dayGetGaps [eventPeriod] onCal
  where
    eventPeriod :: PeriodIntern
    eventPeriod = periodize newEvent

-- TODO: Read in TZ
loadShifts :: IO ShiftWeekTime
loadShifts = validateShiftWeek <$> (decodeFileThrow . (<> "/schedule.yml") =<< configDir)


nameParser :: Text -> ParsecT Text u Identity [String]
nameParser name = traverse (\part -> manyTill anyChar $ string part) nameParts
  where
    nameParts = words . unpack . toLower $ name

eventDuration :: GCalEventI -> (Minutes, HG.Seconds)
eventDuration a = HG.fromSeconds $ HG.timeDiff (inUTC . gCalEnd $ a) (inUTC . gCalStart $ a)
    where
    inUTC = HG.localTimeUnwrap . runIntern

minGapDuration :: Minutes
minGapDuration = 65

totalDuration :: Foldable f => f GCalEventI -> (Minutes, HG.Seconds)
totalDuration = foldr sumMS (0,0)
  where
    sumMS :: GCalEventI -> (Minutes, HG.Seconds) -> (Minutes, HG.Seconds)
    sumMS e t = ((fst (eventDuration e) + fst t), (snd (eventDuration e) + snd t))

-- used to aggregate staffer hours
eventBySummDuration :: Foldable f => f GCalEventI -> [(Text, HG.Hours, Minutes)]
eventBySummDuration  =
  fmap reshapeTime
  . Map.toList
  . foldr bySummary Map.empty
  where
    bySummary :: GCalEvent InternTime -> Map.Map Text (Minutes, HG.Seconds) -> Map.Map Text (Minutes, HG.Seconds)
    bySummary e  =
      Map.insertWith
      sumMS
      (maybe " " (toLower . strip) . gCalSummary $ e)
      $ eventDuration e

    reshapeTime :: (Text, (Minutes, HG.Seconds)) -> (Text, HG.Hours, Minutes)
    reshapeTime (summ, (m, s)) =
      (summ, (fst $ toHours m s), (snd $ toHours m s))

    sumMS :: (Minutes, HG.Seconds) -> (Minutes, HG.Seconds) -> (Minutes, HG.Seconds)
    sumMS (m1, s1) (m2, s2) = (m1 + m2, s1 + s2)

toHours :: Minutes -> HG.Seconds -> (HG.Hours, Minutes)
toHours m s =
  let
    totalSeconds = (HG.toSeconds m) + s
    (hours, remSecs) = HG.fromSeconds totalSeconds :: (HG.Hours, HG.Seconds)
    (mins, _) = HG.fromSeconds remSecs :: (Minutes, HG.Seconds)
  in
    (hours, mins)
