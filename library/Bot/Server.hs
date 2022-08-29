{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fwarn-unused-do-bind #-}

module Bot.Server where

import Bot.Cal
  ( GCalEventI (..),
    gCalDesc,
    gCalEnd,
    gCalStart,
    gCalSummary,
    getAuthToken,
    getEventsForCal,
    getMainLocCalId,
    getMinGapsInRange,
    googleJWT,
    hasOverlap,
    loadShifts,
    mkGCalEvent,
    validateGCalEvent,
    validateGCalEvent',
  )
import Bot.Time
  ( Days (..),
    DisplayMonthDate (..),
    End (..),
    Gaps (..),
    InternTime (..),
    IxGaps (..),
    Period (..),
    PeriodIntern (..),
    RawShiftTime (..),
    ShiftTime (..),
    ShiftWeek (..),
    ShiftWeekTime (..),
    Start (..),
    advanceDate,
    daysUntil,
    daysUntilIter,
    displayTZ,
    getCurrentTime,
    getInternTime',
    getToday,
    internToLocal,
    localOffset,
    show8601UTC,
    validateShiftWeek,
    weekStart,
  )
import Bot.Twilio
  ( Contact (..),
    ContactRole (..),
    configDir,
    loadContacts,
  )
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Lens (Identity, preview, (&), (.~), (?~), (^?))
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
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
import Data.Aeson.Types (Parser)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Char (ord)
import qualified Data.HashMap.Strict as HMap
import qualified Data.Hourglass as HG
import Data.Int (Int64)
import Data.List (find, foldl')
import qualified Data.Map as Map
import Data.Text (Text (..), find, pack, replace, strip, toLower, unpack)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Data.Vector ((!), (!?))
import Data.Yaml (decodeFileThrow, encodeFile)
import LoadEnv (loadEnv)
import qualified Network.URI.Encode as URI
import Network.Wai (requestHeaders)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
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
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty, vsep, (<+>))
import Prettyprinter.Render.Text (renderStrict)
import Servant
  ( Application,
    FormUrlEncoded,
    FromHttpApiData,
    Handler,
    Header,
    Headers,
    Post,
    Proxy (..),
    ReqBody,
    addHeader,
    hoistServer,
    serve,
    (:<|>) (..),
    (:>),
  )
import Servant.Server (ServerT)
import Servant.XML
import System.Environment (getEnv)
import Test.QuickCheck (elements, generate)
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
import Text.RawString.QQ (r)
import Text.Read (readMaybe)
import Web.Cookie
import Web.Cookie (parseCookies)
import Web.HttpApiData (parseHeader)
import Web.Internal.FormUrlEncoded (Form, FromForm (..), unForm)
import Xmlbf (ToXml (..), element, text)

type ShiftSelectMap = Map.Map Int PeriodIntern

type CookiesMap = Map.Map ByteString BSL.ByteString
data TwilioCallResponse = TCallResp Text
data TwilioMsgResponse = TMsgResp Text

data TwilioMsgData = TMsgData {
    msgBody :: Text
  , msgFrom :: Text
  , msgTo :: Text
  }
  deriving Show

data TwilioCallData = TCallData {
    callFrom :: Text
  , callTo :: Text
  }
  deriving Show

type HandlerT = ReaderT TAppState Handler
type TAppState = TVar AppState
data AppState = AppState {
    stateMessage :: Text
  , stateSender :: Text
  }




type API = "sms" :> Header "Cookie" Cookies
                 :> ReqBody '[FormUrlEncoded] TwilioMsgData
                 :> Post '[XML] (Headers '[Header "Set-Cookie" SetCookie] TwilioMsgResponse)
      :<|> "call" :> ReqBody '[FormUrlEncoded] TwilioCallData
                  :> Post '[XML] TwilioCallResponse
      :<|> "backTalk" :> Header "Cookie" Cookies
                      :> ReqBody '[FormUrlEncoded] TwilioMsgData
                      :> Post '[XML] (Headers '[Header "Set-Cookie" SetCookie] TwilioMsgResponse)



server :: ServerT API HandlerT
server = twilMsg
    :<|> twilCall
    :<|> backTalkMsg

  where
    twilMsg :: Maybe Cookies -> TwilioMsgData -> HandlerT (Headers '[Header "Set-Cookie" SetCookie] TwilioMsgResponse)
    twilMsg cookies inMsg = do
      liftIO loadEnv
      contacts <- liftIO loadContacts
      now <- liftIO getCurrentTime
      let inboundNumber = (msgFrom inMsg)
          mContact = Data.List.find ((inboundNumber ==) . contactNumber) contacts
      liftIO (print inMsg)
      case mContact of
        (Just c) ->
          performCmd $ getCmd now inMsg c $ Map.fromList <$> ((fmap . fmap . fmap) BSL.fromStrict cookies)
        Nothing -> do
          liftIO $ print ("Unrecognized number\n" :: Text)
          performCmd UnrecognizedNumber

    twilCall :: TwilioCallData -> HandlerT TwilioCallResponse
    twilCall inMsg = do
      liftIO (print inMsg)
      responses <- liftIO loadCallResponse
      response <- liftIO $ randItem responses
      liftIO . pure . twimlCall $ response

    backTalkMsg :: Maybe Cookies -> TwilioMsgData -> HandlerT (Headers '[Header "Set-Cookie" SetCookie] TwilioMsgResponse)
    backTalkMsg _ msg = do
      state        <- ask
      storedMsg    <- liftIO . atomically . fmap stateMessage $ readTVar state
      storedSender <- liftIO . atomically . fmap stateSender $ readTVar state
      liftIO . atomically $ writeTVar state $ AppState{ stateMessage = msgBody msg, stateSender = msgFrom msg }
      if msgFrom msg == storedSender
        then join . fmap performCmd . fmap SMSResponse . liftIO . dummyBacktalk $ msgFrom msg
        else performCmd $ SMSResponse storedMsg

data Cmd =
    AvailableShifts
  | ClaimShift Contact (Period InternTime)
  | Suspend Contact (Days Int)
  | Help
  | UnrecognizedNumber
  | UnrecognizedCmd TwilioMsgData
  | SMSResponse Text

performCmd :: Cmd -> HandlerT (Headers '[Header "Set-Cookie" SetCookie] TwilioMsgResponse)

performCmd (SMSResponse msg) = pure $ destroyCookie $ twimlMsg msg

performCmd (UnrecognizedCmd d) =
  pure $ destroyCookie (twimlMsg . (\a -> "\"" <> a <> "\" not recognized.\nSay \"info\" for possible commands.\nAnd I don't recommend calling.") . msgBody $ d)

performCmd AvailableShifts = do
  gaps <- indexGaps <$> (liftIO . gapsNowTo . Days $ 7)
  let preMsg = "Open shifts in the next 7 days.\nRespond with the number to claim it.\n"
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

performCmd Help = pure $ destroyCookie $ twimlMsg $ toText helpText


instance ToXml TwilioCallResponse where
  toXml (TCallResp a) =
      element "Response" HMap.empty $
        element "Say" (HMap.fromList [("voice", "woman")]) (text . LText.fromStrict $ a)

instance ToXml TwilioMsgResponse where
  toXml (TMsgResp a) =
      element "Response" HMap.empty $
        element "Message" HMap.empty (text . LText.fromStrict $ a)


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

helpText :: Text
helpText = [r|
Available commands:
"shifts": display 7 days worth of unclaimed shifts.
"suspend (N)": Suspend for number of days (defaults to end of the shift-week)|]

twimlCall :: Text -> TwilioCallResponse
twimlCall = TCallResp

twimlMsg :: Text -> TwilioMsgResponse
twimlMsg = TMsgResp

toText :: Pretty a => a -> Text
toText = textRender . pretty

getCmd :: InternTime -> TwilioMsgData -> Contact -> Maybe CookiesMap -> Cmd

getCmd now d c (Just cm) | (Just period) <- isClaimShift d cm = ClaimShift c period
                         | otherwise = getCmd now d c Nothing

getCmd now d c Nothing | ("shifts":_) <- Text.words . Data.Text.toLower . msgBody $ d = AvailableShifts
                       | Just (Just days) <- parseSuspendDays . toLower . msgBody $ d  = Suspend c days
                       | Just Nothing <- parseSuspendDays . toLower . msgBody $ d  = Suspend c $ daysUntil localOffset now weekStart
                       | True <- parseHelp . toLower . msgBody $ d  = Help
                       | otherwise = UnrecognizedCmd d

-- TODO: the limit on texts in a <Say> twiml node is 4,096.
-- this text is currently 4652
loadCallResponse :: IO [Text]
loadCallResponse = decodeFileThrow . (<> "/call_responses.yml") =<< configDir

randItem :: [a] -> IO a
randItem = generate . elements

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
    todaysDate = fmap HG.dateDay . fmap HG.localTimeUnwrap  $ getToday
    toInt = Prelude.fromIntegral . ord

destroyCookie :: TwilioMsgResponse -> Headers '[Header "Set-Cookie" SetCookie] TwilioMsgResponse
destroyCookie = addHeader emptyAvailableShiftsCookie { setCookieMaxAge = Just 0  }

indexGaps :: Gaps [Period a] -> IxGaps a
indexGaps (Gaps ps) = fst $ foldl' addIx (Map.empty, 1) ps
  where
    addIx (m, i) p = (Map.insert i p m, i + 1)

emptyAvailableShiftsCookie :: SetCookie
emptyAvailableShiftsCookie =
  defaultSetCookie {
    setCookieName = availableShiftsCookieName
  , setCookieValue = ""
  }

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

availableShiftsCookieName :: ByteString
availableShiftsCookieName = "available-shifts"

minGapDuration :: HG.Minutes
minGapDuration = 65

-- TODO: needs instance TOJSON periods a
addAvailableshiftsCookie :: IxGaps InternTime -> TwilioMsgResponse -> Headers '[Header "Set-Cookie" SetCookie] TwilioMsgResponse
addAvailableshiftsCookie m | m == Map.empty = destroyCookie
addAvailableshiftsCookie gaps = do
  let cookie = emptyAvailableShiftsCookie {
        setCookieValue = URI.encodeByteString . BSL.toStrict . Data.Aeson.encode $ gaps
        }
  addHeader cookie

postEvent e = do
  loadEnv
  calId <- getEnv "MAIN_LOC_STAFFING_GCAL_ID"
  token <- getAuthToken =<< googleJWT
  resp <- postWith (defaults & auth ?~ oauth2Bearer token) (url calId) (toJSON e)
  return resp
  where url calID = "https://www.googleapis.com/calendar/v3/calendars/"
          <> calID
          <> "/events"

suspendContact :: Contact -> InternTime -> IO ()
suspendContact c t = do
  oldContacts <- loadContacts
  let cId = contactId c
      newContact = c { contactSuspendUntil = Just t }
      newContacts = newContact : filter ((cId /=) . contactId) oldContacts
  writeContacts newContacts

writeContacts :: [Contact] -> IO ()
writeContacts cs = flip encodeFile cs . (<> "/contacts.yml") =<< configDir

textRender = renderStrict . layoutPretty defaultLayoutOptions

isClaimShift :: TwilioMsgData -> CookiesMap -> Maybe PeriodIntern
isClaimShift d cm = do
  json <- Map.lookup availableShiftsCookieName cm
  shiftmap <- decode . BSL.fromStrict $ URI.decodeByteString . BSL.toStrict $ json :: Maybe ShiftSelectMap
  selected <- Map.lookup <$> (readMaybe . unpack . msgBody $ d) <*> pure shiftmap
  selected

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


instance ToJSON GCalEventI where
  toJSON e = object
    [
      "summary" .= gCalSummary e
    , "description" .= gCalDesc e
    , "start" .= object ["dateTime" .= (show8601UTC $ gCalStart e)]
    , "end" .= object ["dateTime" .= (show8601UTC $ gCalEnd e)]
    ]

instance ToJSON (Period InternTime) where
  toJSON p =
    toJSON [
      periodStart p
    , periodEnd p
    ]

instance ToJSON InternTime where
  toJSON = toJSON . show8601UTC

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

instance ToJSON ContactRole where
  toJSON WeeklyRole = String "weekly"
  toJSON AnnexRole = String "annex"

startServer :: IO ()
startServer = do
  withStdoutLogger $ \appLogger -> do
    state <- atomically $ newTVar AppState{ stateMessage = "hello world!", stateSender = "0" }
    let settings = setPort 3000 $ setLogger (logHeaders appLogger) defaultSettings
    (runSettings $ settings) (mkserver state)

-- -- type ApacheLogger = Request -> Status -> Maybe Integer -> IO ()
-- Use this to log all request headers
logHeaders logger req s i = print (requestHeaders req) >> logger req s i

contactsAPI :: Proxy API
contactsAPI = Proxy

getHandler :: TAppState -> HandlerT a -> Handler a
getHandler = flip runReaderT

mkserver :: TAppState -> Application
mkserver s = serve contactsAPI $ hoistServer contactsAPI (getHandler s) server

instance FromHttpApiData Cookies where
   parseHeader = Right . parseCookies

-- (FromHttpApiData [(ByteString, ByteString)])
