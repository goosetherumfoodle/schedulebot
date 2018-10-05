{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import Debug.Trace

import Text.Read (readMaybe)
import Data.Hourglass (Hours, Minutes)
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

unwrapEvents :: Maybe (Result [GCalEvent]) -> IO [GCalEvent]
unwrapEvents (Just (Success events)) = pure events
unwrapEvents _ = throwIO ResponseErrorGCalEvents

getEvents :: IO [GCalEvent]
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

data GCalDateTime = GCalDateTime String deriving (Show, Eq)

data GCalEvent = GCalEvent
  { gCalSummary :: String
  , gCalDesc :: (Maybe String)
  , gCalStart :: GCalDateTime
  , gCalEnd :: GCalDateTime
  } deriving (Show, Eq)

instance FromJSON GCalDateTime where
  parseJSON = withObject "GCalDateTime" $ \v -> GCalDateTime <$> v .: "dateTime"

instance FromJSON GCalEvent where
  parseJSON = withObject "GCalEvent" $ \o ->
    GCalEvent
      <$> o .: "summary"
      <*> o .:? "description"
      <*> o .: "start"
      <*> o .: "end"

data RawShiftTime = RawShiftTime Int Int deriving (Show, Eq)

data ShiftTime = ShiftTime Hours Minutes

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

timeParser :: ParsecT Text u Identity (String, String)
timeParser = do
  h <- (try $ count 2 digit) <|> count 1 digit
  _ <- char ':'
  m <- count 2 digit
  pure $ (h, m)

instance FromJSON RawShiftTime where
  parseJSON = withText "RawShiftTime" $ \str -> do
    (hrs, mins) <- errHandler $ parse timeParser "timeParser" str
    RawShiftTime <$> readHours hrs <*> readMinutes mins
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
