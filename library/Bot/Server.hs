{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Server where

import Bot.Time (getCurrentTime)
import Bot.Command (performCmd)

import Control.Monad.STM (atomically)
import LoadEnv (loadEnv)
import Control.Monad.IO.Class (liftIO)
import Web.Internal.FormUrlEncoded (Form, unForm, FromForm(..))
import qualified Data.Text.Lazy as LText
import qualified Data.HashMap.Strict as HMap
import Xmlbf (ToXml(..), element, text)
import qualified Data.ByteString.Lazy as BSL
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Servant.XML
import Data.Text (Text(..), find)
import qualified Data.Text as Text
import Web.Cookie
import Servant ( Handler
               , Header
               , Headers
               , Post
               , FormUrlEncoded
               , ReqBody
               , Application
               , Proxy(..)
               , hoistServer
               , addHeader
               , serve
               , (:>)
               , (:<|>)(..))
import Servant.Server (ServerT)
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import qualified Data.Map as Map


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
          mContact = find ((inboundNumber ==) . contactNumber) contacts
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
