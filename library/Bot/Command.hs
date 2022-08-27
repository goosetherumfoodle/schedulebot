module Bot.Command where

import Bot.Twilio (Contact(..), TwilioMsgData(..))
import Bot.Time (Period(..), InternTime)
import Bot.Server (HandlerT)

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
