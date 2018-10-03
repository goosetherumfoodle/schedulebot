{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Example (main) where

import Debug.Trace

import Data.List ((\\), zip)
import Data.Text (Text(..), pack, replace, zip, unpack)
import Data.Text.IO as TXT
import Data.Text.Encoding (encodeUtf8)
import Control.Monad (join)
import Network.Google.OAuth2.JWT
import Network.Wreq
import Control.Lens-- (preview, view)
import Data.Aeson
import Data.Aeson.Lens-- (_String, key)
-- import Data.Aeson.Lens (_String)
import Control.Exception (throwIO, Exception)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Internal as BSLI
import Data.Map as Map hiding ((\\))
import LoadEnv
import System.Environment

main :: IO ()
main = return ()

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
  (getSignedJWT email Nothing ["https://www.googleapis.com/auth/calendar"] Nothing pem) >>= either (throwIO . JWTException) pure

getAuthToken :: SignedJWT -> IO BS.ByteString
getAuthToken jwt = do
  resp <- post "https://www.googleapis.com/oauth2/v4/token" [ "grant_type" := ("urn:ietf:params:oauth:grant-type:jwt-bearer" :: Text), "assertion" := show jwt ]
  mToken <- pure $ unwrapResult $ fmap fromJSON $ resp ^? responseBody . key "access_token"
  maybe (throwIO GAuthTokenException) pure $ traceShow mToken mToken

unwrapResult :: Maybe (Result Text) -> Maybe BS.ByteString
unwrapResult (Just (Success txt)) = Just . encodeUtf8 $ txt
unwrapResult _ = Nothing

testAuthToken :: IO BS.ByteString
testAuthToken = loadEnv >> googleJWT >>= getAuthToken

data GAuthTokenException = GAuthTokenException

instance Exception GAuthTokenException

instance Show GAuthTokenException where
  show GAuthTokenException = "Error fetching google auth token"

getEvents :: IO (Response BSLI.ByteString)
getEvents = do
  loadEnv
  token <- getAuthToken =<< googleJWT
  getWith (defaults & auth ?~ oauth2Bearer token) url
  where url = "https://www.googleapis.com/calendar/v3/calendars/" ++ calId ++ "/events"
        calId = "1j5jbe646s86vm99f15ul91eac@group.calendar.google.com"
