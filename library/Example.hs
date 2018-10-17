{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO:
-- fix msg formatting
-- cron task
-- emergency msg
-- daily nag msg
-- contacts in db
-- deployment

-- DONE:
-- extract sensitive info (#'s) into env and commit

module Example where

--import Debug.Trace

import qualified Data.Map as Map
import Formatting (format, (%))
import Formatting.Formatters (left, ords)
import Data.Text.Prettyprint.Doc (Pretty(..), vsep, (<+>))
import Data.Maybe (catMaybes)
import Data.Int (Int64)
import Data.Yaml (decodeFileThrow)
import Control.Monad.IO.Class (liftIO)
import Twilio (runTwilio')
import Twilio.Messages (PostMessage(..))
import qualified Twilio.Messages as TWIL
import Data.Map (Map)
import Data.List (groupBy)
import Text.Read (readMaybe)
import qualified Data.Hourglass as HG
import Data.Text (Text, toLower, strip, pack, replace, unpack)
import Data.Text.Encoding (encodeUtf8)
import Network.Google.OAuth2.JWT (SignedJWT, fromPEMString, getSignedJWT)
import Control.Lens (Identity, preview, (?~), (&), (^?), (.~))
import Data.Aeson.Types (Parser)
import Data.Aeson.Lens (key)
import Control.Exception (Exception, throwIO)
import Data.ByteString.Char8 (ByteString)
import Data.Vector ((!), (!?))
import LoadEnv (loadEnv)
import System.Environment (getEnv)
import Text.Parsec (ParseError, ParsecT, parse, try, count, digit, char, (<|>))
import Network.Wreq ( FormParam(..)
                    , param
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


newtype JWTException = JWTException String

instance Exception JWTException

instance Show JWTException where
  show (JWTException str) = "Error creating JWT: " <> show str

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

unwrapEvents :: Maybe (Result [Maybe RawGCalEvent]) -> IO [Maybe RawGCalEvent]
unwrapEvents (Just (Success events)) = pure events
unwrapEvents _ = throwIO ResponseErrorGCalEvents

localOffset :: TimezoneOffset
localOffset = TimezoneOffset (-240)

type StartTime = Start InternTime
type EndTime = End InternTime

newtype Start a = Start { runStart :: a } deriving (Functor, Show)
newtype End a = End { runEnd :: a } deriving (Functor, Show)

-- if we don't have the "singleEvents" query param set, then we might get
-- recurring dates before the earliest date we request
getEvents :: (StartTime, EndTime) -> IO [Maybe RawGCalEvent]
getEvents ((Start start), (End end)) = do
  let (fmtMin, fmtMax) = (show8601UTC start, show8601UTC end)
  loadEnv
  calId <- getEnv "GCAL_ID"
  token <- getAuthToken =<< googleJWT
  let reqOpts = defaults
             & auth ?~ oauth2Bearer token
             & param "orderBy" .~ ["startTime"]
             & param "singleEvents" .~ ["true"]
             & param "timeMin" .~ [fmtMin]
             & param "timeMax" .~ [fmtMax]
  resp <- getWith reqOpts $ url calId
  let mEvents = fromJSON <$> preview (responseBody . key "items") resp
  unwrapEvents mEvents
  where url calID = "https://www.googleapis.com/calendar/v3/calendars/"
          <> calID
          <> "/events"

data ResponseErrorGCalEvents = ResponseErrorGCalEvents

instance Exception ResponseErrorGCalEvents

instance Show ResponseErrorGCalEvents where
  show ResponseErrorGCalEvents =
    "Error: No events found in google calendar response body"

type RawGCalEvent = GCalEvent (ISO8601 Text)
type GCalEventI = GCalEvent InternTime

newtype ISO8601 a = ISO8601 a deriving (Show, Eq)

-- todo: refactor to use Period
data GCalEvent a = GCalEvent
  { gCalSummary :: Text
  , gCalDesc :: Maybe Text
  , gCalStart :: a
  , gCalEnd :: a
  } deriving (Show, Eq, Functor)


instance FromJSON (ISO8601 Text) where
  parseJSON = withObject "GCalDateTime" $ \v -> ISO8601 <$> v .: "dateTime"

instance FromJSON (GCalEvent (ISO8601 Text)) where
  parseJSON = withObject "GCalEvent" $ \o ->
    GCalEvent
      <$> o .: "summary"
      <*> o .:? "description"
      <*> o .: "start"
      <*> o .: "end"

validateGCalEvent :: RawGCalEvent -> Either ISO8601Error GCalEventI
validateGCalEvent (GCalEvent smry dsc end st) =
  GCalEvent <$> pure smry <*> pure dsc <*> (parse8601 end) <*> (parse8601 st)

validateGCalEvent' :: RawGCalEvent -> IO GCalEventI
validateGCalEvent' = either throwIO pure . validateGCalEvent


data ISO8601Error = ISO8601Error String

instance Exception ISO8601Error

instance Show ISO8601Error where
  show (ISO8601Error str) =
    "Error: Couldn't parse ISO8601 date format from: " <> str

type RawShiftTime = ShiftTime (Int64, Int64)
type ShiftTimeOfDay = ShiftTime TimeOfDay

data ShiftTime a =
  ShiftTime { shiftTimeTime :: a
            , shiftTimeTZ :: TimezoneOffset
            }
  deriving (Show, Eq)

type ShiftWeekTime = ShiftWeek ShiftTimeOfDay
type ShiftWeekRaw = ShiftWeek RawShiftTime

type InternTime = Intern (LocalTime HG.Elapsed)
type PeriodIntern = Period InternTime

-- ensure that internal time is always UTC
newtype Intern a = Intern { runIntern :: a } deriving (Show, Functor, Ord, Eq)

-- TODO: validate times
validateShiftWeek :: ShiftWeekRaw -> ShiftWeekTime
validateShiftWeek (ShiftW mo tu w th f sa su) =
  ShiftW (toTime mo)
         (toTime tu)
         (toTime w)
         (toTime th)
         (toTime f)
         (toTime sa)
         (toTime su)
  where
    toTime :: [Period RawShiftTime] -> [Period ShiftTimeOfDay]
    toTime = fmap fuck

  -- TODO: clean this up with lenses
    fuck :: Period RawShiftTime -> Period ShiftTimeOfDay
    fuck (Period (ShiftTime (shr, smin) tza) (ShiftTime (ehr, emin) tzb) n) =
      Period  (ShiftTime (hmTime shr smin) tza) (ShiftTime (hmTime ehr emin) tzb) n

    hmTime :: Int64 -> Int64 -> TimeOfDay
    hmTime h m = TimeOfDay (Hours h) (Minutes m) 0 0


data Period a = Period { periodStart :: a
                       , periodEnd :: a
                       , periodName :: Maybe Text
                       } deriving (Show, Eq, Functor)


-- TODO: push TZ from shifttime into shiftweek (we don't want to allow different
-- zones per shift
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
    ShiftTime <$> ((,) <$> readHours hrs <*> readMinutes mins) <*> pure localOffset
      where errHandler :: Either ParseError (String, String) -> Parser (String, String)
            errHandler (Right a) = pure a
            errHandler (Left err) = fail $ "Error parsing hours: " <> show err

            readHours :: Monad m => String -> m Int64
            readHours hrs | Nothing <- readMaybe hrs  :: Maybe Int  = fail $ "Couldn't parse hours"
                          | Just hrsParsed <- readMaybe hrs = pure hrsParsed

            readMinutes :: Monad m => String -> m Int64
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

newtype Gaps a = Gaps a deriving (Show, Eq, Functor)

-- TODO: should daterange be a period?
getGapsInRange :: ShiftWeekTime -> (StartTime, EndTime) -> [GCalEventI] -> Gaps [Period InternTime]
getGapsInRange shifts (start, end) events =
  Gaps $ concat $ fmap (uncurry dayGetGaps) shiftsAndEvents
  where
    shiftsAndEvents = pairShiftsAndEvents datedShifts datedEvents
    datedShifts = shiftsInRange (start, end) shifts
    datedEvents = Map.fromList $ groupByDate events

dayGetGaps :: [Period InternTime] -> [GCalEventI] -> [Period InternTime]
dayGetGaps shifts events = foldr alterGaps shifts events

pairShiftsAndEvents :: [(Date, [PeriodIntern])] -> Map Date [GCalEventI] -> [([PeriodIntern], [GCalEventI])]
pairShiftsAndEvents [] _ = []
pairShiftsAndEvents (d:ds) e =
  case Map.lookup (fst d) e of
    Just events -> (snd d, events) : pairShiftsAndEvents ds e
    Nothing     -> (snd d, []) : pairShiftsAndEvents ds e

type StartDate = Start Date
type EndDate = End Date

-- any shift that *starts* in range
shiftsInRange :: (StartTime, EndTime) -> ShiftWeekTime -> [(Date, [PeriodIntern])]
shiftsInRange (start, end) shifts = filter hasShifts allDatesInRange
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
      uncurry datesFromTo (timeGetDate <$> start, timeGetDate <$> end)

    datesShifts :: ShiftWeekTime -> Date -> (Date, [PeriodIntern])
    datesShifts s date = (,) date
      $ periodTimeToElapsed date
      <$> pickShiftSelector (getWeekDay date) s

datesFromTo :: StartDate -> EndDate -> [Date]
datesFromTo (Start d1) (End d2)
  | d1 < d2   = d1 : datesFromTo (Start . nextDate $ d1) (End d2)
  | otherwise = [d2]

nextDate :: Date -> Date
nextDate = flip dateAddPeriod day
  where
    day = HG.Period
      { HG.periodYears = 0
      , HG.periodMonths = 0
      , HG.periodDays = 1
      }

groupByDate :: [GCalEventI] -> [(Date, [GCalEventI])]
groupByDate = fmap addDate . groupBy (\a b -> eventDate a == eventDate b)
  where
    addDate :: [GCalEventI] -> (Date, [GCalEventI])
    addDate events = ((eventDate . head $ events), events)
    eventDate = dtDate . timeGetDateTimeOfDay . gCalStart

periodTimeToElapsed :: Date -> Period ShiftTimeOfDay -> PeriodIntern
periodTimeToElapsed date a =
  Period
  (tODToIntern . periodStart $ a)
  (tODToIntern . periodEnd   $ a)
  (periodName a)
  where
    tODToIntern :: ShiftTime TimeOfDay -> InternTime
    tODToIntern (ShiftTime t z) = getInternTime z (mkDateTime t)

    mkDateTime = DateTime date

alterGaps :: GCalEventI -> [PeriodIntern] -> [PeriodIntern]
alterGaps _ [] = []
alterGaps event gaps@(g:gs) =
  case gapRel g event of
    PostGap           -> gaps
    PriorGap          -> g : alterGaps event gs
    SubsetGap         -> alterGaps event gs
    StrictSupersetGap -> splitGap g event <> alterGaps event gs
    IntersectGap      -> shrinkGap g event : alterGaps event gs

data GapRelationship =
    PostGap
  | PriorGap
  | SubsetGap
  | StrictSupersetGap
  | IntersectGap
  deriving (Show, Eq)

splitGap :: PeriodIntern -> GCalEventI -> [PeriodIntern]
splitGap g e = [
    Period (periodStart g) (gCalStart e) Nothing
  , Period (gCalEnd e) (periodEnd g) Nothing
  ]

shrinkGap :: PeriodIntern -> GCalEventI -> PeriodIntern
shrinkGap g e | periodStart g < gCalStart e = Period (periodStart g) (gCalStart e) Nothing
              | otherwise                   = Period (gCalEnd e) (periodEnd g) Nothing

gapRel :: PeriodIntern -> GCalEventI -> GapRelationship
gapRel g e
  | periodEnd g <= gCalStart e = PriorGap
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

sendSms :: Text -> IO ()
sendSms msg = do
  loadEnv
  to <- pack <$> getEnv "TO_NUMBER"
  from <- pack <$> getEnv "FROM_NUMBER"
  runTwilio' (getEnv "TWILIO_ACCT_SID") (getEnv "TWILIO_AUTH_TOKEN") $ do
    resp <- TWIL.post $ PostMessage to from msg
    liftIO $ print resp

-- TODO: Read in TZ
loadShifts :: IO ShiftWeekTime
loadShifts = validateShiftWeek <$> decodeFileThrow "schedule.yml"

testGapAlert :: Int -> Int -> IO ()
testGapAlert from to = do
  let startDate = DateTime (Date { dateDay = from, dateMonth = October, dateYear = 2018 }) $ TimeOfDay 0 0 0 0
      endDate = DateTime (Date { dateDay = to, dateMonth = October, dateYear = 2018 }) $ TimeOfDay 23 58 58 0
      start = Start $ internTimeFromLocalOffset startDate
      end = End $ internTimeFromLocalOffset endDate
  sched <- loadShifts
  rawEvents <- catMaybes <$> getEvents (start, end)
  events <- validateGCalEvent' `traverse` rawEvents
  let gaps = getGapsInRange sched (start, end) events
  print $ pretty $ (fmap . fmap . fmap) internToLocal gaps

eventsInRange :: StartTime -> EndTime -> IO [GCalEventI]
eventsInRange start end = do
  rawEvents <- catMaybes <$> getEvents (start, end)
  events <- validateGCalEvent' `traverse` rawEvents
  pure events

summaryDurationsInRange :: StartTime -> EndTime -> IO [(Text, Hours, Minutes)]
summaryDurationsInRange s e = eventBySummDuration <$> eventsInRange s e

-- ensure that we convert to correct TZ for display purposes
newtype DisplayTZ a = DisplayTZ { runDTZ :: a } deriving (Functor)

-- track that number is a date for display
newtype DisplayDate a = DisplayDate { runDD :: a } deriving (Functor)

displayTZ :: (HG.Time a) => TimezoneOffset -> LocalTime a -> DisplayTZ (LocalTime a)
displayTZ tz = DisplayTZ . HG.localTimeSetTimezone tz

internToDisplay :: TimezoneOffset -> InternTime -> DisplayTZ (LocalTime HG.Elapsed)
internToDisplay tz = DisplayTZ . HG.localTimeSetTimezone tz . runIntern

internToLocal :: InternTime -> DisplayTZ (LocalTime HG.Elapsed)
internToLocal = internToDisplay localOffset

instance Pretty a => Pretty (Gaps a) where
  pretty (Gaps a) = vsep ["Found gaps: ", pretty a]

instance Pretty (Period (DisplayTZ (LocalTime HG.Elapsed))) where
  pretty (Period s _ (Just name)) = pretty (timeGetDate <$> s) <+> pretty name
  pretty (Period s e Nothing)     = pretty (timeGetDate <$> s) <+> pretty s <+> "-" <+> pretty e

instance Pretty (DisplayTZ Date) where
  pretty d = pretty  (getWeekDay <$> d) <+> pretty (DisplayDate . dateDay <$> d)

instance Pretty (DisplayTZ WeekDay) where
  pretty (DisplayTZ Monday)    = "Mon"
  pretty (DisplayTZ Tuesday)   = "Tues"
  pretty (DisplayTZ Wednesday) = "Wed"
  pretty (DisplayTZ Thursday)  = "Thurs"
  pretty (DisplayTZ Friday)    = "Fri"
  pretty (DisplayTZ Saturday)  = "Sat"
  pretty (DisplayTZ Sunday)    = "Sun"

instance Pretty (DisplayTZ (DisplayDate Int)) where
  pretty (DisplayTZ (DisplayDate a)) = pretty $ format ords a

instance Pretty (DisplayTZ (LocalTime HG.Elapsed)) where
  pretty (DisplayTZ e) =
    pretty $ DisplayTZ time
    where
      time = HG.timeGetTimeOfDay . HG.localTimeUnwrap $ e

instance Pretty (DisplayTZ TimeOfDay) where
  pretty t = pretty $ format (left 2 '0' % ":" % left 2 '0')
             ((\(Hours h) -> h) . todHour . runDTZ $ t)
             ((\(Minutes m) -> m) . todMin . runDTZ $ t)

instance Pretty (Concise (DisplayTZ Month)) where
  pretty (Concise (DisplayTZ January))  = "Jan"
  pretty (Concise (DisplayTZ February)) = "Feb"
  pretty (Concise (DisplayTZ March))    = "Mar"
  pretty (Concise (DisplayTZ April))    = "Apr"
  pretty (Concise (DisplayTZ May))      = "May"
  pretty (Concise (DisplayTZ June))     = "Jun"
  pretty (Concise (DisplayTZ July))     = "Jul"
  pretty (Concise (DisplayTZ August))   = "Aug"
  pretty (Concise (DisplayTZ September))= "Sep"
  pretty (Concise (DisplayTZ October))  = "Oct"
  pretty (Concise (DisplayTZ November)) = "Nov"
  pretty (Concise (DisplayTZ December)) = "Dec"

newtype Concise a = Concise a

getInternTime :: Timeable t => TimezoneOffset -> t -> InternTime
getInternTime currentTZ =
  Intern
  . HG.localTimeSetTimezone utcOffset
  . HG.localTime currentTZ
  . HG.timeGetElapsed

internTimeFromUTC :: Timeable t => t -> InternTime
internTimeFromUTC = getInternTime utcOffset

internTimeFromLocalOffset :: Timeable t => t -> InternTime
internTimeFromLocalOffset = getInternTime localOffset

utcOffset :: TimezoneOffset
utcOffset = TimezoneOffset $ timezoneOffset UTC

-- I'm assuming that this correctly handles the timezone
parse8601 :: ISO8601 Text -> Either ISO8601Error InternTime
parse8601 (ISO8601 s) =
  toEither maybeIntern
  where
    toEither = maybe (Left . ISO8601Error $ unpack s) Right
    maybeIntern = internTimeFromUTC . HG.timeGetElapsed
                  <$> HG.timeParse ISO8601_DateAndTime (unpack s)

show8601 :: TimezoneOffset -> InternTime -> Text
show8601 tzo = pack
  . HG.localTimePrint ISO8601_DateAndTime
  . HG.localTimeSetTimezone tzo
  . runIntern

show8601UTC :: InternTime -> Text
show8601UTC =  show8601 utcOffset

show8601Local :: InternTime -> Text
show8601Local = show8601 localOffset

instance Timeable (LocalTime HG.Elapsed) where
  timeGetElapsedP = HG.localTimeToGlobal . (flip HG.ElapsedP 0 <$>)

instance Timeable InternTime where
  timeGetElapsedP = HG.timeGetElapsedP . runIntern

eventDuration :: GCalEventI -> (Minutes, HG.Seconds)
eventDuration a = fromSeconds $ timeDiff (gCalEnd a) (gCalStart a)

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
      (toLower . strip . gCalSummary $ e)
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
