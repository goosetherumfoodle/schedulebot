{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Twilio where

import Data.Text (Text)
import Data.Set (Set(..))
import Bot.Time (InternTime(..))

import Web.Internal.FormUrlEncoded (Form, unForm, FromForm(..))
import Xmlbf (ToXml(..), element, text)
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text.Lazy as LText

data TwilioCallResponse = TCallResp Text
data TwilioMsgResponse = TMsgResp Text


data TwilioCallData = TCallData {
    callFrom :: Text
  , callTo :: Text
  }
  deriving Show

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

-- ensure we don't message inactive contacts
newtype Active a = Active { runActive :: a } deriving (Show, Eq)



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
