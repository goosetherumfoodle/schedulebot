{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fwarn-unused-do-bind #-}


-- TODO:
-- show annex event names in alerts
-- claiming annex shifts
-- check for ruby pairity
-- figure out how to handle suspensions for both locations
-- use phone number as ID
-- improve cmd matching
-- improve handling for all-day events (recognize and toss)
-- handle DST
-- port 1877
-- limit max suspension days
-- task for verifying env vars and config files
-- split <say> nodes into <gather> parent
-- test parsing contacts.yaml
-- consider how/when to number gaps (for selection) (or to make more like ruby version)
-- consider rendering shift select list from same data as shifts json (first make a map?)
-- consider switching to megaparsec
-- contacts in db
-- announcement command
-- error logging/reporting
-- handle DST and read TZ from config file
-- push env vars (and other?) into State monad
-- only create gauth token when old one expires

-- DONE:
-- write alerts for annex location
-- user roles
-- alert messages filter by corresponding roles
-- suspend response should show month
-- default suspension days if no number specified
-- "commands" (help) command
-- test task for who will be msged by nagger
-- deployment
-- cron tasks
-- expose notification tasks
-- tasks leave alone suspended users
-- fix adding extra contacts on write
-- "shifts" should look at next 7 days
-- suspensions
-- post exlusivly (not if another event exists)
-- claim gcal shifts
-- fixz correspondance between shift select map and displayed shifts
-- implement twilio msg conversation
-- capture twilio cookies
-- destroying cookies
-- set twilio cookies
-- api endpoint and msg response
-- contacts in yaml
-- emergency msg
-- daily nag msg
-- fix msg formatting
-- extract sensitive info (#'s) into env and commit

module Example where

--import Debug.Trace

import Prelude hiding (until)

import Data.Foldable (asum)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.RawString.QQ (r)
import Data.List (foldl', find)
import qualified Network.URI.Encode as URI
import Xmlbf (ToXml(..), element', text)
import Web.Cookie
import Web.HttpApiData -- (ToHttpApiData(..))
import qualified Data.HashMap.Strict as HMap
import Servant.XML
import Web.Internal.FormUrlEncoded (Form, unForm, FromForm(..))
import Servant.Server (Server)
import Network.Wai (requestHeaders)
import Network.Wai.Handler.Warp (setLogger, setPort, runSettings, defaultSettings)
import Network.Wai.Logger (withStdoutLogger)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Time.System (localDateCurrentAt)
import qualified Data.Map as Map
import Formatting (format, (%))
import qualified Formatting.Formatters as FMT
import Formatting.Formatters (left, int)
import Data.Text.Prettyprint.Doc (Pretty(..), defaultLayoutOptions, layoutPretty, vsep, (<+>))
import Data.Maybe (catMaybes)
import Data.Int (Int64)
import Data.Yaml (decodeFileThrow, encodeFile)
import Control.Monad.IO.Class (liftIO)
import Twilio (runTwilio')
import Twilio.Messages (PostMessage(..))
import qualified Twilio.Messages as TWIL
import Data.Map (Map)
import Data.List (groupBy)
import Text.Read (readMaybe)
import qualified Data.Hourglass as HG
import Data.Text (Text, toLower, strip, pack, replace, unpack)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Network.Google.OAuth2.JWT (SignedJWT, fromPEMString, getSignedJWT)
import Control.Lens (Identity, preview, (?~), (&), (^?), (.~))
import Data.Aeson.Types (Parser)
import Data.Aeson.Lens (key)
import Control.Exception (Exception, throwIO)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Vector ((!), (!?))
import LoadEnv (loadEnv)
import System.Environment (getEnv)
import Servant ( Handler
               , Header
               , Headers
               , Post
               , FormUrlEncoded
               , ReqBody
               , Application
               , Proxy(..)
               , addHeader
               , serve
               , (:>)
               , (:<|>)(..))
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

type InternTime = Intern (LocalTime HG.Elapsed)

newtype Days a = Days a deriving (Show, Eq)
newtype DisplayMonthDate a = DisplayMonthDate a
newtype Concise a = Concise a
newtype JWTException = JWTException String
newtype Start a = Start { runStart :: a } deriving (Functor, Show, Eq)
newtype End a = End { runEnd :: a } deriving (Functor, Show, Eq)
newtype CalId a = CalId a
newtype ISO8601 a = ISO8601 a deriving (Show, Eq)
newtype Gaps a = Gaps { runGaps :: a } deriving (Show, Eq, Functor)
-- ensure that internal time is always UTC
newtype Intern a = Intern { runIntern :: a } deriving (Show, Functor, Ord, Eq)
newtype Covered a = Covered { runCovered :: a } deriving (Functor, Foldable, Traversable, Show)
newtype StoreEvent a = StoreEvent { runStoreEvent :: a } deriving (Functor, Foldable, Traversable, Show)
-- ensure we don't message inactive contacts
newtype Active a = Active { runActive :: a } deriving (Show, Eq)
-- ensure that we convert to correct TZ for display purposes
newtype DisplayTZ a = DisplayTZ { runDTZ :: a } deriving (Functor, Eq, Show)
-- track that number is a date for display
newtype DisplayDate a = DisplayDate { runDD :: a } deriving (Functor)

data TwilioMsgData = TMsgData {
    msgBody :: Text
  , msgFrom :: Text
  , msgTo :: Text
  }
  deriving Show

data ContactRole = WeeklyRole | AnnexRole deriving (Show, Eq, Ord)

data Contact = Contact { contactName :: Text
                       , contactNumber :: Text
                       , contactSuspendUntil :: Maybe InternTime
                       , contactId :: Int
                       , contactRoles :: Set ContactRole
                       }
             deriving (Show, Eq)

data Cmd =
    AvailableShifts
  | ClaimShift Contact (Period InternTime)
  | Suspend Contact (Days Int)
  | Help
  | UnrecognizedNumber
  | UnrecognizedCmd TwilioMsgData

data TwilioCallResponse = TCallResp Text
data TwilioMsgResponse = TMsgResp Text
data Period a = Period { periodStart :: a
                       , periodEnd :: a
                       , periodName :: Maybe Text
                       } deriving (Show, Eq, Functor)

data GapRelationship =
    PostGap
  | PriorGap
  | SubsetGap
  | StrictSupersetGap
  | IntersectGap
  deriving (Show, Eq)

data ShiftTime a =
  ShiftTime { stTime :: a
            , stZone :: TimezoneOffset
            }
  deriving (Show, Eq, Functor)

-- TODO: push TZ from shifttime into shiftweek (we don't want to allow different
-- zones per shift
data ShiftWeek a = ShiftW
  { wkMon :: [Period a]
  , wkTue :: [Period a]
  , wkWed :: [Period a]
  , wkThu :: [Period a]
  , wkFri :: [Period a]
  , wkSat :: [Period a]
  , wkSun :: [Period a]
  }
  deriving (Show, Eq)

data GAuthTokenException = GAuthTokenException

data ResponseErrorGCalEvents = ResponseErrorGCalEvents String
-- todo: refactor to use Period

data GCalEvent a = GCalEvent
  { gCalSummary :: Maybe Text
  , gCalDesc :: Maybe Text
  , gCalStart :: a
  , gCalEnd :: a
  } deriving (Show, Eq, Functor)
data ISO8601Error = ISO8601Error String

data TwilioCallData = TCallData {
    callFrom :: Text
  , callTo :: Text
  }
  deriving Show

type CookiesMap = Map ByteString BSL.ByteString
type ShiftSelectMap = Map Int PeriodIntern
type IxGaps a = Map Int (Period a)
type StartDate = Start Date
type EndDate = End Date
type ShiftWeekTime = ShiftWeek ShiftTimeOfDay
type ShiftWeekRaw = ShiftWeek RawShiftTime
type PeriodIntern = Period InternTime
type RawShiftTime = ShiftTime (Int64, Int64)
type ShiftTimeOfDay = ShiftTime TimeOfDay
type RawGCalEvent = GCalEvent (ISO8601 Text)
type GCalEventI = GCalEvent InternTime
type StartTime = Start InternTime
type EndTime = End InternTime

-- instance Bitraversable ShiftTime where

instance FromJSON InternTime where
  parseJSON = withText "InternTime" $ \txt ->
    either (fail . show) pure $ parse8601 $ ISO8601 txt

instance ToXml TwilioCallResponse where
  toXml (TCallResp a) =
    [
      element' "Response" HMap.empty
      [
        element' "Say" (HMap.fromList [("voice", "woman")]) ([text a])
      ]
    ]

instance ToXml TwilioMsgResponse where
  toXml (TMsgResp a) =
    [
      element' "Response" HMap.empty
      [
        element' "Message" HMap.empty ([text a])
      ]
    ]

instance FromForm TwilioMsgData where
  fromForm = handleErr . maybeTData
    where
      maybeTData :: Form -> Maybe TwilioMsgData
      maybeTData f = do
        let m = unForm f
        body <- HMap.lookup "Body" m
        from <- HMap.lookup "From" m
        to   <- HMap.lookup "To"   m
        pure $ TMsgData (head body) (head from) (head to)

      handleErr = maybe (Left "Failure parsing twilio message form-data") Right

instance FromForm TwilioCallData where
  fromForm = handleErr . maybeTData
    where
      maybeTData :: Form -> Maybe TwilioCallData
      maybeTData f = do
        let m = unForm f
        from <- HMap.lookup "From" m
        to   <- HMap.lookup "To"   m
        pure $ TCallData (head from) (head to)

      handleErr = maybe (Left "Failure parsing twilio call form-data") Right

instance Pretty (IxGaps (DisplayTZ (LocalTime HG.Elapsed))) where --TODO: handle empty
  pretty im = vsep $ fmap prettyIx $ Map.toList im
    where
      prettyIx (i, p) = pretty i <> ":" <+> pretty p

instance (Pretty a) => Pretty (Gaps [a]) where
  pretty (Gaps []) = "All shifts covered this week!"
  pretty (Gaps a) = vsep $ ["Open shifts: "]
                            <> fmap pretty a

instance Pretty (Period (DisplayTZ (LocalTime HG.Elapsed))) where
  pretty (Period s _ (Just name)) = pretty ((internToDate <$> HG.localTimeGetTimezone <*> Intern) <$> s) <+> pretty name
  pretty (Period s e Nothing)     = pretty ((internToDate <$> HG.localTimeGetTimezone <*> Intern) <$> s) <+> pretty s
                                    <+> "to" <+> pretty e

instance Pretty (DisplayTZ Date) where
  pretty d = pretty  (getWeekDay <$> d)

instance Pretty (DisplayMonthDate (DisplayTZ (LocalTime Date))) where
  pretty (DisplayMonthDate d) =
    pretty (Concise $ dateMonth . HG.localTimeUnwrap <$> d)
    <+> pretty (DisplayDate . dateDay . HG.localTimeUnwrap <$> d)

instance Pretty (DisplayTZ (LocalTime Date)) where
  pretty = pretty . fmap HG.localTimeUnwrap

instance Pretty (DisplayTZ WeekDay) where
  pretty (DisplayTZ Monday)    = "Mon"
  pretty (DisplayTZ Tuesday)   = "Tue"
  pretty (DisplayTZ Wednesday) = "Wed"
  pretty (DisplayTZ Thursday)  = "Thu"
  pretty (DisplayTZ Friday)    = "Fri"
  pretty (DisplayTZ Saturday)  = "Sat"
  pretty (DisplayTZ Sunday)    = "Sun"

instance Pretty (DisplayTZ (DisplayDate Int)) where
  pretty (DisplayTZ (DisplayDate a)) = pretty $ format int a

instance Pretty (DisplayTZ (LocalTime HG.Elapsed)) where
  pretty (DisplayTZ e) =
    pretty $ DisplayTZ $ toTime e
    where
      toTime = dtTime . HG.timeGetDateTimeOfDay . HG.localTimeUnwrap

  -- TODO: clean up
instance Pretty (DisplayTZ TimeOfDay) where
  pretty (DisplayTZ t) = do
    let formattedTime =
          if minutes == 0
          then format (FMT.int ) hoursFmt
          else format (FMT.int % ":" % left 2 '0' ) hoursFmt minutes

    pretty $ formattedTime

    where
      hoursFmt = if (todHour t) <= 12
             then (\(Hours a) -> a) (todHour t)
             else (\(Hours a) -> a) (todHour t) - 12

      minutes = (\(Minutes a) -> a) (todMin t)

      -- meridian = if (todHour t) < 12
      --            then "AM"
      --            else "PM"

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

instance FromHttpApiData Cookies where
   parseHeader = Right . parseCookies

instance FromJSON ContactRole where
  parseJSON = withText "ContactRole" $ \t ->
    case t of
      "weekly" -> pure WeeklyRole
      "annex" -> pure AnnexRole
      _ -> fail "expecting a valid contact role"

instance ToJSON ContactRole where
  toJSON WeeklyRole = String "weekly"
  toJSON AnnexRole = String "annex"

instance Exception GAuthTokenException

instance Show GAuthTokenException where
  show GAuthTokenException = "Error fetching google auth token"

instance Exception JWTException

instance Show JWTException where
  show (JWTException str) = "Error creating JWT: " <> show str

instance FromJSON RawShiftTime where
  parseJSON = withText "RawShiftTime" $ \str -> do
    (hrs, mins) <- errHandler $ parse clockTimeParser "timeParser" str
    ShiftTime <$> ((,) <$> readHours hrs <*> readMinutes mins) <*> pure localOffset
      where errHandler :: Either ParseError (String, String) -> Parser (String, String)
            errHandler (Right a) = pure a
            errHandler (Left err) = fail $ "Error parsing hours: " <> show err

            readHours :: Monad m => String -> m Int64
            readHours hrs | Just hrsParsed <- readMaybe hrs = pure hrsParsed
                          | otherwise = fail $ "Couldn't parse hours"

            readMinutes :: Monad m => String -> m Int64
            readMinutes mins | Just minsParsed <- readMaybe mins = pure minsParsed
                             | otherwise = fail $ "Couldn't parse minutes"

instance FromJSON a => FromJSON (Period a) where
  parseJSON = withArray "Period" $ \ary ->
    Period
    <$> (parseJSON $ ary ! 0)
    <*> (parseJSON $ ary ! 1)
    <*> (sequence $ parseJSON <$> ary !? 2)

instance ToJSON (Period InternTime) where
  toJSON p =
    toJSON [
      periodStart p
    , periodEnd p
    ]

instance ToJSON InternTime where
  toJSON = toJSON . show8601UTC

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

instance Exception ISO8601Error

instance Show ISO8601Error where
  show (ISO8601Error str) =
    "Error: Couldn't parse ISO8601 date format from: " <> str

instance Exception ResponseErrorGCalEvents

instance Show ResponseErrorGCalEvents where
  show (ResponseErrorGCalEvents a) =
    "Error: in google calandar response: " <> show a


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

instance FromJSON a => FromJSON (GCalEvent a) where
  parseJSON = withObject "GCalEvent" $ \o ->
    GCalEvent
      <$> o .:? "summary"
      <*> o .:? "description"
      <*> o .: "start"
      <*> o .: "end"

instance ToJSON GCalEventI where
  toJSON e = object
    [
      "summary" .= gCalSummary e
    , "description" .= gCalDesc e
    , "start" .= object ["dateTime" .= (show8601UTC $ gCalStart e)]
    , "end" .= object ["dateTime" .= (show8601UTC $ gCalEnd e)]
    ]

instance FromJSON Contact where
  parseJSON = withObject "Contact" $ \o ->
    Contact
    <$> o .: "name"
    <*> o .: "number"
    <*> o .: "suspendUntil"
    <*> o .: "id"
    <*> o .: "roles"

instance ToJSON Contact where
  toJSON (Contact name number susp cid roles) = object [
      "name"   .= name
    , "number" .= number
    , "suspendUntil" .= susp
    , "id" .= cid
    , "roles" .= roles
    ]

  toEncoding (Contact name number susp cid roles) =
    pairs ("name" .= name
           <> "number" .= number
           <> "suspendUntil" .= susp
           <> "id" .= cid
           <> "roles" .= roles
          )

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


unwrapEvents :: Maybe (Result [Maybe RawGCalEvent]) -> IO [Maybe RawGCalEvent]
unwrapEvents (Just (Success events)) = pure events
unwrapEvents err = throwIO $ ResponseErrorGCalEvents $ show err

localOffset :: TimezoneOffset
localOffset = TimezoneOffset (-240)

getInternTime :: Timeable t => TimezoneOffset -> t -> InternTime
getInternTime currentTZ =
  Intern
  . HG.localTimeSetTimezone utcOffset
  . HG.localTime currentTZ
  . HG.timeGetElapsed

getInternTime' :: Timeable t => LocalTime t -> InternTime
getInternTime' lt =
  getInternTime (HG.localTimeGetTimezone lt) $ HG.localTimeUnwrap lt

internTimeFromUTC :: Timeable t => t -> InternTime
internTimeFromUTC = getInternTime utcOffset

internTimeFromLocalOffset :: Timeable t => t -> InternTime
internTimeFromLocalOffset = getInternTime localOffset

utcOffset :: TimezoneOffset
utcOffset = TimezoneOffset $ timezoneOffset UTC

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

postEvent e = do
  loadEnv
  calId <- getEnv "GCAL_ID"
  token <- getAuthToken =<< googleJWT
  resp <- postWith (defaults & auth ?~ oauth2Bearer token) (url calId) (toJSON e)
  return resp
  where url calID = "https://www.googleapis.com/calendar/v3/calendars/"
          <> calID
          <> "/events"

validateGCalEvent :: RawGCalEvent -> Either ISO8601Error GCalEventI
validateGCalEvent (GCalEvent smry dsc end st) =
  GCalEvent
  <$> pure (toLower . strip <$> smry)
  <*> pure (toLower . strip <$> dsc)
  <*> (parse8601 end)
  <*> (parse8601 st)

validateGCalEvent' :: RawGCalEvent -> IO GCalEventI
validateGCalEvent' = either throwIO pure . validateGCalEvent

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
    toTime = fmap . fmap . fmap $ uncurry mkTOD

    mkTOD :: Int64 -> Int64 -> TimeOfDay
    mkTOD h m = TimeOfDay (Hours h) (Minutes m) 0 0

suspendDaysParser :: ParsecT Text u Identity (Maybe (Maybe (Days Int)))
suspendDaysParser = do
  spaces
  _ <- string "suspend"
  spaces
  d <- (readMaybe :: String -> Maybe Int) <$> many digit
  pure $ pure $ Days <$> d

parseSuspendDays :: Text -> Maybe (Maybe (Days Int))
parseSuspendDays t = handler $ parse suspendDaysParser "suspendDays" (toLower t)
  where
    handler = either (const Nothing) id

parseHelp :: Text -> Bool
parseHelp t = handler $ parse helpParser "Help" (toLower t)
  where
    handler = either (const False) (const True)

helpParser :: ParsecT Text u Identity ()
helpParser = do
  spaces
  _ <- string "command"
  spaces
  pure ()

clockTimeParser :: ParsecT Text u Identity (String, String)
clockTimeParser = do
  h <- (try $ count 2 digit) <|> count 1 digit
  _ <- char ':'
  m <- count 2 digit
  pure $ (h, m)

contains :: ParsecT Text () Identity [String] -> Text -> Bool
contains p input = toBool $ parse p "contains" (toLower input)
  where
    toBool = either (const False) (const True)

nameParser :: Text -> ParsecT Text u Identity [String]
nameParser name = traverse (\part -> manyTill anyChar $ string part) nameParts
  where
    nameParts = words . unpack . toLower $ name

minGapDuration :: Minutes
minGapDuration = 65

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
getAllGapsAllDay tzo coverage events =
  Gaps $ dayGetGaps storeEvents coveragePeriods
  where
    storeEvents = periodize . runStoreEvent <$> events
    coveragePeriods = runCovered <$> coverage

-- TODO: Generalize this logic, as we're now using it in more places than just
-- shifts (from a schedule) and events (meaning covered shifts)
-- maybe use some newtypes related to openings and coverage
dayGetGaps :: [Period InternTime] -> [GCalEventI] -> [Period InternTime]
dayGetGaps shifts events = foldr alterGaps shifts events

pairShiftsAndEvents :: [(Date, [PeriodIntern])] -> Map Date [GCalEventI] -> [([PeriodIntern], [GCalEventI])]
pairShiftsAndEvents [] _ = []
pairShiftsAndEvents (d:ds) e =
  case Map.lookup (fst d) e of
    Just events -> (snd d, events) : pairShiftsAndEvents ds e
    Nothing     -> (snd d, []) : pairShiftsAndEvents ds e

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
      <$> pickShiftSelector (getWeekDay date) s

datesFromTo :: StartDate -> EndDate -> [Date]
datesFromTo (Start d1) (End d2)
  | d1 < d2   = d1 : datesFromTo (Start . nextDate $ d1) (End d2)
  | otherwise = [d2]

advanceDate :: Days Int -> Date -> Date
advanceDate (Days n) =
  flip dateAddPeriod day
  where
    day = HG.Period
      { HG.periodYears = 0
      , HG.periodMonths = 0
      , HG.periodDays = n
      }

nextDate :: Date -> Date
nextDate = advanceDate . Days $ 1

groupByDate :: TimezoneOffset -> [GCalEventI] -> [(Date, [GCalEventI])]
groupByDate tzo = fmap addDate . groupBy (\a b -> eventDate a == eventDate b)
  where
    addDate :: [GCalEventI] -> (Date, [GCalEventI])
    addDate events = ((eventDate . head $ events), events)
    eventDate = internToDate tzo . gCalStart

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

sendSmsToRole :: ContactRole -> [Active Contact] -> Text -> IO ()
sendSmsToRole role cs msg=
  sendSmsTo (filter (Set.member role . contactRoles . runActive) cs)
            msg

sendSmsTo :: [Active Contact] -> Text -> IO ()
sendSmsTo cs msg = do
  loadEnv
  from <- pack <$> getEnv "FROM_NUMBER"
  twilSid <- getEnv "TWILIO_ACCT_SID"
  twilAuth <- getEnv "TWILIO_AUTH_TOKEN"
  let activeContacts = runActive <$> cs
  print =<< traverse (msgEach twilSid twilAuth msg from . unpack . contactNumber) activeContacts
  where
    msgEach sid au msg' from num = runTwilio' (pure sid) (pure au) $ do
      resp <- TWIL.post $ PostMessage (pack num) from msg'
      pure resp

sendSms :: Text -> IO ()
sendSms msg = do
  loadEnv
  to <- pack <$> getEnv "TO_NUMBER"
  from <- pack <$> getEnv "FROM_NUMBER"
  runTwilio' (getEnv "TWILIO_ACCT_SID") (getEnv "TWILIO_AUTH_TOKEN") $ do
    resp <- TWIL.post $ PostMessage to from msg
    liftIO $ print resp

loadContacts :: IO [Contact]
loadContacts = decodeFileThrow . (<> "/contacts.yml") =<< configDir

configDir :: IO String
configDir = loadEnv >> getEnv "CONFIG_DIR"

-- TODO: Read in TZ
loadShifts :: IO ShiftWeekTime
loadShifts = validateShiftWeek <$> (decodeFileThrow . (<> "/schedule.yml") =<< configDir)

weekStart :: WeekDay
weekStart = Wednesday

weekOf :: Date -> (StartDate, EndDate)
weekOf d
  | getWeekDay d == weekStart = (Start d, End $ advanceDate (Days 6) d)
  | otherwise = ((Start $ prevWeekStart d), (End $ advanceDate (Days 6) $ prevWeekStart d ))

prevWeekStart :: Date -> Date
prevWeekStart d
  | getWeekDay d == weekStart = d
  | otherwise = prevWeekStart $ advanceDate (Days (-1)) d

getMainLocCalId :: IO (CalId String)
getMainLocCalId = CalId <$> getEnv "MAIN_LOC_STAFFING_GCAL_ID"

getAnnexEventsCalId :: IO (CalId String)
getAnnexEventsCalId = CalId <$> getEnv "ANNEX_EVENTS_GCAL_ID"

getAnnexName :: IO Text
getAnnexName = pack <$> getEnv "ANNEX_NAME"

getMainLocName :: IO Text
getMainLocName = pack <$> getEnv "MAIN_LOC_NAME"

getAnnexStaffingCalId :: IO (CalId String)
getAnnexStaffingCalId = CalId <$> getEnv "ANNEX_STAFFING_GCAL_ID"

-- TODO: fix issue with sending this late night (TZoffset issue?)
mainLocEmergencySMS :: IO ()
mainLocEmergencySMS = do
  loadEnv
  calId    <- getMainLocCalId
  name    <- getMainLocName
  today    <- getInternTime' <$> getToday
  tomorrow <- getTomorrow
  contacts <- onlyActive today <$> loadContacts
  let tomorrowEOD = flip DateTime (TimeOfDay 23 59 59 0) <$> tomorrow
      start = Start $ getInternTime' tomorrow
      end = End $ getInternTime' tomorrowEOD
      tzo = localOffset
  sched <- loadShifts
  rawEvents <- getEventsForCal calId (start, end)
  events <- validateGCalEvent' `traverse` rawEvents
  let gaps = getMinGapsInRange tzo minGapDuration sched (start, end) events
  if not . null. runGaps $ gaps
    then let preMsg = name <> " shift not covered tomorrow!\n"
             msg = renderGaps gaps
             postMsg = "\nRespond with \"shifts\" to claim one right now"
         in
           print ("sending emergency alert to: " <> (pack . show $ contacts))
           >> sendSmsTo contacts (preMsg <> msg <> postMsg)
    else print ("no gaps tomorrow" :: Text)

onlyActive :: InternTime -> [Contact] -> [Active Contact]
onlyActive now =  fmap Active . filter (pastSuspDate . contactSuspendUntil)
  where
    pastSuspDate Nothing = True
    pastSuspDate (Just suspDate) | suspDate <= now = True
                                 | otherwise = False

-- TODO: extract and test more of this logic
testNagAlert :: Text -> IO ()
testNagAlert staffer = do
  loadEnv
  calId <- getMainLocCalId
  today <- getToday
  let
    todayTZ = HG.localTimeGetTimezone today
    tzo = localOffset
    (_, endDate) = weekOf $ HG.localTimeUnwrap today
    endDateEOD = flip DateTime (TimeOfDay 23 59 59 0) <$> endDate
    start = getInternTime' <$> Start today
    end = getInternTime' <$> HG.localTime todayTZ <$> endDateEOD
  sched <- loadShifts
  rawEvents <- getEventsForCal calId (start, end)
  events <- validateGCalEvent' `traverse` rawEvents
  if stafferOnCal staffer events
    then print ("What a responsible bastard!" :: Text)
    else let gaps = getMinGapsInRange tzo minGapDuration sched (start, end) events
             gapMsg = pack $ show $ renderGaps gaps
         in print $ "couldn't find " <> staffer <> pack " on cal\n" <> gapMsg

-- TODO: extract and test logic
nagAlert :: IO ()
nagAlert = do
  loadEnv
  calId <- getMainLocCalId
  print "Nagger notifications starting"
  today <- getToday
  let
    tzo = localOffset
    todayTZ = HG.localTimeGetTimezone today
    (startDate, endDate) = weekOf $ HG.localTimeUnwrap today
    startDateBOD = flip DateTime (TimeOfDay 0 0 0 0) <$> startDate
    endDateEOD = flip DateTime (TimeOfDay 23 59 59 0) <$> endDate
    start = getInternTime' <$> HG.localTime todayTZ <$> startDateBOD
    end = getInternTime' <$> HG.localTime todayTZ <$> endDateEOD
    currentDayIntern = getInternTime' today
  sched <- loadShifts
  rawEvents <- getEventsForCal calId  (start, end)
  events <- validateGCalEvent' `traverse` rawEvents
  contacts <- onlyActive currentDayIntern <$> loadContacts
  let gaps = getMinGapsInRange tzo minGapDuration sched (start, end) events
      toNag = whomToNag events contacts
  if null . runGaps $ gaps
    then pure () -- send no alerts if there are no gaps
    else foldr (msgLaxContacts gaps) (pure ()) toNag
  where
      msgLaxContacts gaps c _ =
        let name = contactName . runActive $ c
            gapMsg = pack $ show $ renderGaps gaps
            preMsg = "\"" <> name <> "\" isn't on the cal yet this week. Pls take a shift (or say \"suspend\" if you can't this week)\n"
            postMsg = "\nRespond with \"shifts\" to claim one right now"
        in print ("couldn't find " <> name <> " on cal")
           >> sendSmsToRole WeeklyRole [c] (preMsg <> gapMsg <> postMsg)

whomToNag :: [GCalEventI] -> [Active Contact] -> [Active Contact]
whomToNag es = filter $ not . flip stafferOnCal es . contactName . runActive

stafferOnCal :: Foldable f => Text -> f (GCalEvent a) -> Bool
stafferOnCal staffer = any (maybe False hasName . gCalSummary)
  where
    hasName = contains $ nameParser staffer

-- TODO: extract and test more of this logic
-- TODO: don't return gaps from a previous shift today
-- TODO: fix Pretty instance for gaps
gapsThisWeekAlert :: IO ()
gapsThisWeekAlert = do
  loadEnv
  mainName <- getMainLocName
  calId <- getMainLocCalId
  today <- getToday
  contacts <- onlyActive (getInternTime' today) <$> loadContacts
  let
    dateToStart =
      if isEndOfWeek today
      then nextDate <$> today
      else today
    tzo = localOffset -- TODO: push tzo into state monad
    todayTZ = HG.localTimeGetTimezone today -- TODO: consider removing all the "todayTZ" instances
    (_, endDate) = weekOf $ HG.localTimeUnwrap dateToStart
    endDateEOD = flip DateTime (TimeOfDay 23 59 59 0) <$> endDate
    start = getInternTime' <$> Start today
    end = getInternTime' <$> HG.localTime todayTZ <$> endDateEOD
  sched <- loadShifts
  rawEvents <- getEventsForCal calId (start, end)
  events <- validateGCalEvent' `traverse` rawEvents
  let gaps = getMinGapsInRange tzo minGapDuration sched (start, end) events
  sendSmsToRole WeeklyRole contacts $ ((mainName <> " Weekly Schedule Notice:\n") <>) $ renderGaps gaps
  where
    isEndOfWeek = (== weekStart) . getWeekDay . nextDate . HG.localTimeUnwrap

annexGapsThisWeekAlert :: IO ()
annexGapsThisWeekAlert = do
  loadEnv
  annexName <- getAnnexName
  eventCalId <- getAnnexEventsCalId
  staffingCalId <- getAnnexStaffingCalId
  today <- getToday
  contacts <- onlyActive (getInternTime' today) <$> loadContacts
  let
    dateToStart =
      if isEndOfWeek today
      then nextDate <$> today
      else today
    tzo = localOffset -- TODO: push tzo into state monad
    todayTZ = HG.localTimeGetTimezone today -- TODO: consider removing all the "todayTZ" instances
    (_, endDate) = weekOf $ HG.localTimeUnwrap dateToStart
    endDateEOD = flip DateTime (TimeOfDay 23 59 59 0) <$> endDate
    start = getInternTime' <$> Start today
    end = getInternTime' <$> HG.localTime todayTZ <$> endDateEOD
  rawEvents <- (fmap.fmap) StoreEvent $ getEventsForCal eventCalId (start, end)
  events <- (traverse . traverse) validateGCalEvent' rawEvents
  rawCoverage <- (fmap.fmap) Covered $ getEventsForCal staffingCalId (start, end)
  coverage <- (traverse . traverse) validateGCalEvent' rawCoverage
  let gaps = getAllGapsAllDay tzo coverage events
  sendSmsToRole AnnexRole contacts $ ((annexName <> " Schedule:\n") <>) $ renderGaps gaps
  where
    isEndOfWeek = (== weekStart) . getWeekDay . nextDate . HG.localTimeUnwrap

renderGaps :: Gaps [Period InternTime] -> Text
renderGaps = toText . (fmap . fmap . fmap) internToLocal

annexEmergencySMS :: IO ()
annexEmergencySMS = do
  loadEnv
  annexName <- getAnnexName
  eventCalId <- getAnnexEventsCalId
  staffingCalId <- getAnnexStaffingCalId
  today    <- getInternTime' <$> getToday
  tomorrow <- getTomorrow
  contacts <- onlyActive today <$> loadContacts
  let tomorrowEOD = flip DateTime (TimeOfDay 23 59 59 0) <$> tomorrow
      start = Start $ getInternTime' tomorrow
      end = End $ getInternTime' tomorrowEOD
      tzo = localOffset
  rawEvents <- (fmap.fmap) StoreEvent $ getEventsForCal eventCalId (start, end)
  events <- (traverse . traverse) validateGCalEvent' rawEvents
  rawCoverage <- (fmap.fmap) Covered $ getEventsForCal staffingCalId (start, end)
  coverage <- (traverse . traverse) validateGCalEvent' rawCoverage
  let gaps = getAllGapsAllDay tzo coverage events
  if not . null . runGaps $ gaps
    then let preMsg = annexName <> " events not staffed tomorrow!:\n"
             msg = renderGaps gaps
         in
           (print (annexName <> ": sending emergency alert to: " <> (pack . show $ contacts)))
            >> (sendSmsToRole AnnexRole contacts $ preMsg <> msg)
    else print (annexName <> ": no gaps tomorrow" :: Text)

textRender = renderStrict . layoutPretty defaultLayoutOptions

toText :: Pretty a => a -> Text
toText = textRender . pretty

-- TODO: remove this (it was just here for debugging)
-- TODO: Replace with test of Pretty instace for gaps
displayGaps :: Gaps [Period (DisplayTZ (LocalTime HG.Elapsed))] -> Text
displayGaps = toText

-- this should eventually be in a state monad
getToday :: IO (LocalTime Date)
getToday = (fmap . fmap . fmap) dtDate localDateCurrentAt utcOffset

getCurrentTime :: IO InternTime
getCurrentTime = getInternTime' <$> localDateCurrentAt utcOffset

-- -- for fun
-- getTodayStub :: IO (LocalTime Date)
-- getTodayStub = pure $ HG.localTime localOffset date
--   where
--     date = Date { dateDay = 26, dateMonth = October, dateYear = 2018 }

getTomorrow :: IO (LocalTime Date)
getTomorrow = (fmap . fmap) nextDate $ getToday

-- TODO: save these somewhere for calculations
-- eventsInRange :: StartTime -> EndTime -> IO [GCalEventI]
-- eventsInRange start end = do
--   rawEvents <- getEvents (start, end)
--   events <- validateGCalEvent' `traverse` rawEvents
--   pure events

-- summaryDurationsInRange :: StartTime -> EndTime -> IO [(Text, Hours, Minutes)]
-- summaryDurationsInRange s e = eventBySummDuration <$> eventsInRange s e

displayTZ :: (HG.Time a) => TimezoneOffset -> LocalTime a -> DisplayTZ (LocalTime a)
displayTZ tz = DisplayTZ . HG.localTimeSetTimezone tz

internToDisplay :: TimezoneOffset -> InternTime -> DisplayTZ (LocalTime HG.Elapsed)
internToDisplay tz = DisplayTZ . HG.localTimeSetTimezone tz . runIntern

internToLocal :: InternTime -> DisplayTZ (LocalTime HG.Elapsed)
internToLocal = internToDisplay localOffset

indexGaps :: Gaps [Period a] -> IxGaps a
indexGaps (Gaps ps) = fst $ foldl' addIx (Map.empty, 1) ps
  where
    addIx (m, i) p = (Map.insert i p m, i + 1)

internToDateTime :: TimezoneOffset -> InternTime -> DateTime
internToDateTime tzo = timeGetDateTimeOfDay . HG.localTimeUnwrap . HG.localTimeSetTimezone tzo . runIntern

internToDate :: TimezoneOffset -> InternTime -> Date
internToDate tzo = dtDate . internToDateTime tzo

periodDuration :: PeriodIntern -> (Minutes, HG.Seconds)
periodDuration a = fromSeconds $ timeDiff (inUTC . periodEnd $ a) (inUTC . periodStart $ a)
  where
    inUTC = HG.localTimeUnwrap . runIntern

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

type API = "sms" :> Header "Cookie" Cookies
                 :> ReqBody '[FormUrlEncoded] TwilioMsgData
                 :> Post '[XML] (Headers '[Header "Set-Cookie" SetCookie] TwilioMsgResponse)
      :<|> "call" :> ReqBody '[FormUrlEncoded] TwilioCallData
                  :> Post '[XML] TwilioCallResponse

server :: Server API
server = twilMsg
    :<|> twilCall

  where
    twilMsg :: Maybe Cookies -> TwilioMsgData -> Handler (Headers '[Header "Set-Cookie" SetCookie] TwilioMsgResponse)
    twilMsg cookies inMsg = do
      liftIO loadEnv
      contacts <- liftIO loadContacts
      now <- liftIO getCurrentTime
      let inboundNumber = (msgFrom inMsg)
          mContact = find ((inboundNumber ==) . contactNumber) contacts
      liftIO (print inMsg)
      case mContact of
        (Just c) ->
          performCmd $ getCmd now inMsg c $ Map.fromList <$> ((fmap . fmap . fmap) BSL.fromStrict cookies)
        Nothing -> do
          liftIO $ print ("Unrecognized number\n" :: Text)
          performCmd UnrecognizedNumber

    twilCall :: TwilioCallData -> Handler TwilioCallResponse
    twilCall inMsg = liftIO (print inMsg) >> pure (twimlCall bob)

twimlCall :: Text -> TwilioCallResponse
twimlCall = TCallResp

twimlMsg :: Text -> TwilioMsgResponse
twimlMsg = TMsgResp

isClaimShift :: TwilioMsgData -> CookiesMap -> Maybe PeriodIntern
isClaimShift d cm = do
  json <- Map.lookup availableShiftsCookieName cm
  shiftmap <- decode . BSL.fromStrict $ URI.decodeByteString . BSL.toStrict $ json :: Maybe ShiftSelectMap
  selected <- Map.lookup <$> (readMaybe . unpack . msgBody $ d) <*> pure shiftmap
  selected

getCmd :: InternTime -> TwilioMsgData -> Contact -> Maybe CookiesMap -> Cmd

getCmd now d c (Just cm) | (Just period) <- isClaimShift d cm = ClaimShift c period
                         | otherwise = getCmd now d c Nothing

getCmd now d c Nothing | ("shifts":_) <- Text.words . toLower . msgBody $ d = AvailableShifts
                       | Just (Just days) <- parseSuspendDays . toLower . msgBody $ d  = Suspend c days
                       | Just Nothing <- parseSuspendDays . toLower . msgBody $ d  = Suspend c $ daysUntil localOffset now weekStart
                       | True <- parseHelp . toLower . msgBody $ d  = Help
                       | otherwise = UnrecognizedCmd d

daysUntil :: TimezoneOffset -> InternTime -> WeekDay -> (Days Int)
daysUntil tzo now end =
  Days $ daysUntilIter 0 dateInZone end
  where
    dateInZone =
      timeGetDate . HG.localTimeUnwrap . HG.localTimeSetTimezone tzo . runIntern $ now

daysUntilIter :: Int -> Date -> WeekDay -> Int
daysUntilIter counter day end | (getWeekDay day) == end = counter
                              | otherwise = daysUntilIter (counter + 1) (nextDate day) end

performCmd :: Cmd -> Handler (Headers '[Header "Set-Cookie" SetCookie] TwilioMsgResponse)

performCmd (UnrecognizedCmd d) =
  pure $ destroyCookie (twimlMsg . (\a -> "\"" <> a <> "\" not recognized") . msgBody $ d)

performCmd AvailableShifts = do
  gaps <- indexGaps <$> (liftIO . gapsNowTo . Days $ 7)
  let preMsg = "Open shifts this week.\nRespond with the number to claim it.\n"
  pure
    $ addAvailableshiftsCookie
    gaps
    (twimlMsg . (preMsg <>) . toText . (fmap . fmap) internToLocal $ gaps)

performCmd UnrecognizedNumber = do
  pure $ destroyCookie $ twimlMsg ""

performCmd (ClaimShift contact period) = do
  calId <- liftIO getMainLocCalId
  mEvents <- liftIO $ (fmap . fmap) validateGCalEvent
                    $ getEventsForCal calId
                     ((Start $ periodStart period), (End $ periodEnd period))
  let available = not
                  <$> (hasOverlap <$> (sequence mEvents)
                                  <*> pure (mkGCalEvent period ""))
  case available of
    Right True ->
      liftIO $ print <$> postEvent (mkGCalEvent period (contactName contact))
      >> (pure $ destroyCookie $ twimlMsg $ "Shift claimed: " <> (toText $ internToLocal <$> period))
    _ ->
      pure $ destroyCookie $ twimlMsg $ "That shift is no longer available. :( Try again."

performCmd (Suspend c d) = do
  today <- liftIO getToday
  let until = advanceDate d <$> today
      msg = pretty ("Notifications silenced until: " :: Text)
            <> (pretty . DisplayMonthDate . displayTZ localOffset $ until)
  liftIO $ suspendContact c $ getInternTime' until
  pure $ destroyCookie $ twimlMsg $ textRender $ msg

performCmd Help =
  pure $ destroyCookie $ twimlMsg $ toText helpText


suspendContact :: Contact -> InternTime -> IO ()
suspendContact c t = do
  oldContacts <- loadContacts
  let cId = contactId c
      newContact = c { contactSuspendUntil = Just t }
      newContacts = newContact : filter ((cId /=) . contactId) oldContacts
  writeContacts newContacts

writeContacts :: [Contact] -> IO ()
writeContacts cs = flip encodeFile cs . (<> "/contacts.yml") =<< configDir

mkGCalEvent :: PeriodIntern -> Text -> GCalEventI
mkGCalEvent p summ = GCalEvent (Just summ) botText (periodStart p) (periodEnd p)
  where
    botText = Just "Event created by shift bot."

hasOverlap :: [GCalEventI] -> GCalEventI -> Bool
hasOverlap onCal newEvent = ([eventPeriod] /=) $ dayGetGaps [eventPeriod] onCal
  where
    eventPeriod :: PeriodIntern
    eventPeriod = periodize newEvent

periodize :: GCalEventI -> PeriodIntern
periodize e = Period (gCalStart e) (gCalEnd e) Nothing

-- TODO: needs instance TOJSON periods a
addAvailableshiftsCookie :: IxGaps InternTime -> TwilioMsgResponse -> Headers '[Header "Set-Cookie" SetCookie] TwilioMsgResponse
addAvailableshiftsCookie m | m == Map.empty = destroyCookie
addAvailableshiftsCookie gaps = do
  let cookie = emptyAvailableShiftsCookie {
        setCookieValue = URI.encodeByteString . BSL.toStrict . encode $ gaps
        }
  addHeader cookie

addAvailableshiftsCookie' :: ToJSON (Gaps [a]) => Gaps [a] -> TwilioMsgResponse -> Headers '[Header "Set-Cookie" SetCookie] TwilioMsgResponse
addAvailableshiftsCookie' (Gaps []) = destroyCookie
addAvailableshiftsCookie' gaps = do
  let cookie = emptyAvailableShiftsCookie {
        setCookieValue = URI.encodeByteString . BSL.toStrict . encode $ gaps
        }
  addHeader cookie

destroyCookie :: TwilioMsgResponse -> Headers '[Header "Set-Cookie" SetCookie] TwilioMsgResponse
destroyCookie = addHeader emptyAvailableShiftsCookie { setCookieMaxAge = Just 0  }

emptyAvailableShiftsCookie :: SetCookie
emptyAvailableShiftsCookie =
  defaultSetCookie {
    setCookieName = availableShiftsCookieName
  , setCookieValue = ""
  }

availableShiftsCookieName :: ByteString
availableShiftsCookieName = "available-shifts"

contactsAPI :: Proxy API
contactsAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve contactsAPI server

startServer :: IO ()
startServer = do
  withStdoutLogger $ \appLogger -> do
    let settings = setPort 3000 $ setLogger (logHeaders appLogger) defaultSettings
    runSettings settings app

-- -- type ApacheLogger = Request -> Status -> Maybe Integer -> IO ()
-- Use this to log all request headers
logHeaders logger req s i = print (requestHeaders req) >> logger req s i

gapsNowTo :: Days Int -> IO (Gaps [Period InternTime])
gapsNowTo d = do
  loadEnv
  calId <- getMainLocCalId
  today <- getToday
  let
    tzo = localOffset
    endDateEOD = (fmap.fmap) (advanceDate d) $ End today
    start = getInternTime' <$> Start today
    end = getInternTime' <$> endDateEOD
  sched <- loadShifts
  rawEvents <- getEventsForCal calId (start, end)
  events <- validateGCalEvent' `traverse` rawEvents
  pure $ getMinGapsInRange tzo minGapDuration sched (start, end) events

helpText :: Text
helpText = [r|
Available commands:
"shifts": display 7 days worth of unclaimed shifts.
"suspend (N)": Suspend for number of days (defaults to end of the shift-week)|]






















































-- TODO: the limit on texts in a <Say> twiml node is 4,096.
-- this text is currently 4652
bob :: Text
bob =  "In astronomy, \"revolution\" refers to a return to the same place. For the left it seems to mean about the same. Leftism is literally reactionary. Just as generals prepare to fight the last war, leftists incite the last revolution. They welcome it because they know it failed. They are vanguardists because they are always behind the times. Like all leaders, leftists are least obnoxious when following their following, but in certain crises they step to the fore to make the system work. If the left/right metaphor has any meaning, it can only be that the left is to the left of the same thing the right is to the right of. But what if revolution means stepping out of line? If there were no right, the left would have to invent it — and it often has. (Examples: Calculated hysteria over Nazis and the K K K which awards these wimpy slugs the notoriety they need; or lowest-common-denunciation of the Moral Majority obviating unmannerly attacks on the real sources of moralist tyranny — the family, religion in general, and the work-ethic espoused by leftists and Christians alike.) The right likewise needs the left: its operational definition is always anti-communism, variously drecked-out. Thus left and right presuppose and re-create each other. One bad thing about bad times is that they make opposition too easy, as (for instance) the current economic crisis gets shoehorned into archaic Marxist, populist or syndicalist categories. The left thereby positions itself to fulfill its historic role as reformer of those incidental (albeit agonizing) evils which, properly attended to, conceal the system's essential inequities: hierarchy, moralism, bureaucracy, wage-labor, monogamy, government, money. (How can Marxism ever be more than capitals most sophisticated way of thinking about itself?) Consider the acknowledged epicenter of the current crisis. work. Unemployment is a bad thing. But it doesn't follow, outside of righto-leftist dogma, that employment is a good thing. It is not. The \"right to work,\" arguably an appropriate slogan in 1848, is obsolete in 1982. People don't need work. What we need is satisfaction of subsistence requirements, on the one hand, and opportunities for creative, convivial, educative, diverse, passionate activity on the other. Twenty years ago the Goodman brothers guessed that 5% of the labor then expended would meet minimum survival needs, a figure which must be lower today; obviously entire so-called industries serve nothing but the predatory purposes of commerce and coercion. That's an ample infrastructure to play with in creating a world of freedom, community and pleasure where \"production\" of use-values is \"consumption\" of free gratifying activity. Transforming work into play is a project for a proletariat that refuses that condition, not for leftists left with nothing to lead. Pragmatism, as is obvious from a glance at its works, is a delusive snare. Utopia is sheer common sense. The choice between \"full employment\" and unemployment — the choice that left and right collaborate to confine us to — is the choice between the Gulag and the gutter. No wonder that after all these years a stifled and suffering populace is weary of the democratic lie. There are less and less people who want to work, even among those who rightly fear unemployment, and more and more people who want to work wonders. By all means let's agitate for handouts, tax cuts, freebies, bread and circuses — why not bite the hand that feeds you? the flavor is excellent — but without illusions. The surrational kernel of truth in the mystical Marxist shell is this: the \"working class\" is the legendary \"revolutionary agent\": but only if, by not working, it abolishes class. Perennial \"organizers,\" leftists don't understand that the workers have already been definitively \"organized\" by, and can only be organized for — their bosses. \"Activism\" is idiocy if it enriches and empowers our enemies. Leftism, that parasite for sore eyes, dreads the outbreak of a Wilhelm Reichstag fire which will consume its parties and unions along with the corporations and armies and churches currently controlled by its ostensible opposite. Nowadays you have to be odd to get even. Greylife leftism, with its checklists of obligatory antagonisms (to this-ism, that-ism and the other-ism: everything but leftism) is devoid of all humor and imagination: hence it can stage only coups, not revolutions, which change lies but not life. But the urge to create is also a destructive urge. One more effort, leftists, if you would be revolutionaries! If you're not revolting against work, you're working against revolt."
