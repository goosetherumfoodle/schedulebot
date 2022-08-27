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



type CookiesMap = Map ByteString BSL.ByteString
type ShiftSelectMap = Map Int PeriodIntern

-- instance Bitraversable ShiftTime where

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

            readHours :: MonadFail m => String -> m Int64
            readHours hrs | Just hrsParsed <- readMaybe hrs = pure hrsParsed
                          | otherwise = fail $ "Couldn't parse hours"

            readMinutes :: MonadFail m => String -> m Int64
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
  calId <- getEnv "MAIN_LOC_STAFFING_GCAL_ID"
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
  _ <- string "commands"
       <|> string "help"
       <|> string "info"
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

sendSmsToRole :: ContactRole -> [Active Contact] -> Text -> IO ()
sendSmsToRole role cs msg =
  sendSmsTo (filter (Set.member role . contactRoles . runActive) cs) msg

sendSmsTo :: [Active Contact] -> Text -> IO ()
sendSmsTo cs msg = do
  loadEnv
  from     <- pack <$> getEnv "FROM_NUMBER"
  twilSid  <- getEnv "TWILIO_ACCT_SID"
  twilAuth <- getEnv "TWILIO_AUTH_TOKEN"
  let activeContacts = runActive <$> cs
  print =<< traverse (msgEach twilSid twilAuth msg from . unpack . contactNumber) activeContacts
  where
    msgEach sid au msg' from num = runTwilio' (pure sid) (pure au) $ do
      resp <- TWIL.post $ PostMessage (pack num) from msg' Nothing
      pure resp

sendSms :: Text -> IO ()
sendSms msg = do
  loadEnv
  to <- pack <$> getEnv "TO_NUMBER"
  from <- pack <$> getEnv "FROM_NUMBER"
  runTwilio' (getEnv "TWILIO_ACCT_SID") (getEnv "TWILIO_AUTH_TOKEN") $ do
    resp <- TWIL.post $ PostMessage to from msg Nothing
    liftIO $ print resp

configDir :: IO String
configDir = loadEnv >> getEnv "CONFIG_DIR"

-- TODO: Read in TZ
loadShifts :: IO ShiftWeekTime
loadShifts = validateShiftWeek <$> (decodeFileThrow . (<> "/schedule.yml") =<< configDir)

weekStart :: WeekDay
weekStart = Sunday

weekOf :: Date -> (StartDate, EndDate)
weekOf d
  | getWeekDay d == weekStart = (Start d, End $ advanceDate (Days 6) d)
  | otherwise = ((Start $ prevWeekStart d), (End $ advanceDate (Days 6) $ prevWeekStart d ))

prevWeekStart :: Date -> Date
prevWeekStart d
  | getWeekDay d == weekStart = d
  | otherwise = prevWeekStart $ advanceDate (Days (-1)) d


loadContacts :: IO [Contact]
loadContacts = decodeFileThrow . (<> "/contacts.yml") =<< configDir

stafferOnCal :: Foldable f => Text -> f (GCalEvent a) -> Bool
stafferOnCal staffer = any (maybe False hasName . gCalSummary)
  where
    hasName = contains $ nameParser staffer

textRender = renderStrict . layoutPretty defaultLayoutOptions

toText :: Pretty a => a -> Text
toText = textRender . pretty

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


indexGaps :: Gaps [Period a] -> IxGaps a
indexGaps (Gaps ps) = fst $ foldl' addIx (Map.empty, 1) ps
  where
    addIx (m, i) p = (Map.insert i p m, i + 1)

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

dummyBacktalk :: Text -> IO Text
dummyBacktalk from = do
  dummies <- decodeFileThrow . (<> "/dummy_responses.yml") =<< configDir
  date    <- todaysDate
  fudge   <- randItem [0, 3]
  let xs  = fmap toInt . unpack $ from
  let x   = date + sum xs + fudge
      i   = x `mod` length dummies
  pure $ dummies !! i

  where
    todaysDate = fmap dateDay . fmap HG.localTimeUnwrap  $ getToday
    toInt = Prelude.fromIntegral . ord

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

suspendContact :: Contact -> InternTime -> IO ()
suspendContact c t = do
  oldContacts <- loadContacts
  let cId = contactId c
      newContact = c { contactSuspendUntil = Just t }
      newContacts = newContact : filter ((cId /=) . contactId) oldContacts
  writeContacts newContacts

writeContacts :: [Contact] -> IO ()
writeContacts cs = flip encodeFile cs . (<> "/contacts.yml") =<< configDir

hasOverlap :: [GCalEventI] -> GCalEventI -> Bool
hasOverlap onCal newEvent = ([eventPeriod] /=) $ dayGetGaps [eventPeriod] onCal
  where
    eventPeriod :: PeriodIntern
    eventPeriod = periodize newEvent

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

getHandler :: TAppState -> HandlerT a -> Handler a
getHandler = flip runReaderT

mkserver :: TAppState -> Application
mkserver s = serve contactsAPI $ hoistServer contactsAPI (getHandler s) server

startServer :: IO ()
startServer = do
  withStdoutLogger $ \appLogger -> do
    state <- atomically $ newTVar AppState{ stateMessage = "hello world!", stateSender = "0" }
    let settings = setPort 3000 $ setLogger (logHeaders appLogger) defaultSettings
    (runSettings $ settings) (mkserver state)

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

randItem :: [a] -> IO a
randItem = generate . elements

-- TODO: the limit on texts in a <Say> twiml node is 4,096.
-- this text is currently 4652
loadCallResponse :: IO [Text]
loadCallResponse = decodeFileThrow . (<> "/call_responses.yml") =<< configDir
