{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fwarn-unused-do-bind #-}

module Bot.Alert where

import Bot.Cal
  ( CalId (..),
    GCalEvent (..),
    GCalEventI (..),
    StoreEvent (..),
    getAllGapsAllDay,
    getEventsForCal,
    getMinGapsInRange,
    loadShifts,
    minGapDuration,
    validateGCalEvent',
  )
import Bot.Time
  ( Covered (..),
    DateTime (..),
    DisplayTZ,
    Elapsed,
    End (..),
    Gaps (..),
    InternTime (..),
    LocalTime,
    Period (..),
    ShiftWeekTime (..),
    Start (..),
    TimeOfDay (..),
    getInternTime',
    getToday,
    getTomorrow,
    internToLocal,
    localOffset,
    localTimeGetTimezone,
    localTimeUnwrap,
    nextDate,
    validateShiftWeek,
    weekOf,
    weekStart,
  )
import Bot.Twilio
  ( Active (..),
    Contact (..),
    ContactRole (..),
    loadContacts,
  )
import Control.Lens (Identity, preview, (&), (.~), (?~), (^?))
import Control.Monad.IO.Class (liftIO)
import Data.Hourglass (localTime)
import qualified Data.Hourglass as HG
import Data.Int (Int64)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text (..), pack, replace, strip, toLower, unpack)
import Data.Yaml (decodeFileThrow, encodeFile)
import LoadEnv (loadEnv)
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty, vsep, (<+>))
import Prettyprinter.Render.Text (renderStrict)
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
import Twilio (runTwilio')
import Twilio.Messages (PostMessage (..))
import qualified Twilio.Messages as TWIL

mainLocEmergencySMS :: IO ()
mainLocEmergencySMS = do
  loadEnv
  calId    <- getMainLocCalId
  name     <- getMainLocName
  today    <- getInternTime' <$> getToday
  tomorrow <- getTomorrow
  contacts <- onlyActive today <$> loadContacts
  let tomorrowEOD = flip HG.DateTime (HG.TimeOfDay 23 59 59 0) <$> tomorrow
      start       = Start $ getInternTime' tomorrow
      end         = End $ getInternTime' tomorrowEOD
      tzo         = localOffset
  sched     <- loadShifts
  rawEvents <- getEventsForCal calId (start, end)
  events    <- validateGCalEvent' `traverse` rawEvents
  let gaps = getMinGapsInRange tzo minGapDuration sched (start, end) events
  if not . null. runGaps $ gaps
    then let preMsg  = name <> " shift not covered tomorrow!\n"
             msg     = renderGaps gaps
             postMsg = "\nRespond with \"shifts\" to claim one right now"
         in
           print ("sending emergency alert to: " <> (pack . show $ contacts))
           >> sendSmsTo contacts (preMsg <> msg <> postMsg)
    else print ("no gaps tomorrow" :: Text)

-- TODO: extract and test logic
nagAlert :: IO ()
nagAlert = do
  loadEnv
  calId <- getMainLocCalId
  print ("Nagger notifications starting" :: Text)
  today <- getToday
  let
    tzo                  = localOffset
    todayTZ              = localTimeGetTimezone today
    (startDate, endDate) = weekOf $ localTimeUnwrap today
    startDateBOD         = flip HG.DateTime (HG.TimeOfDay 0 0 0 0) <$> startDate
    endDateEOD           = flip HG.DateTime (HG.TimeOfDay 23 59 59 0) <$> endDate
    start                = getInternTime' <$> localTime todayTZ <$> startDateBOD
    end                  = getInternTime' <$> localTime todayTZ <$> endDateEOD
    currentDayIntern     = getInternTime' today
  sched     <- loadShifts
  rawEvents <- getEventsForCal calId  (start, end)
  events    <- validateGCalEvent' `traverse` rawEvents
  contacts  <- onlyActive currentDayIntern <$> loadContacts
  let gaps  = getMinGapsInRange tzo minGapDuration sched (start, end) events
      toNag = whomToNag events contacts
  if null . runGaps $ gaps
    then print ("NAGGING: Nobody, as no gaps were found" :: Text) -- send no alerts if there are no gaps
    else mapM_ (msgLaxContacts gaps) toNag
  where
      msgLaxContacts gaps c =
        let name    = contactName . runActive $ c
            gapMsg  = renderGaps gaps
            preMsg  = "\"" <> name <> "\" isn't on the cal yet this week. Pls take a shift (or say \"suspend\" if you can't this week)\n"
            postMsg = "\nRespond with \"shifts\" to claim one right now"
        in print ("\nNAGGING:  " <> name)
           >> sendSmsToRole WeeklyRole [c] (preMsg <> gapMsg <> postMsg)

-- TODO: extract and test more of this logic
-- TODO: don't return gaps from a previous shift today
-- TODO: fix Pretty instance for gaps
gapsThisWeekAlert :: IO ()
gapsThisWeekAlert = do
  loadEnv
  mainName <- getMainLocName
  calId    <- getMainLocCalId
  today    <- getToday
  contacts <- onlyActive (getInternTime' today) <$> loadContacts
  let
    dateToStart  = if isEndOfWeek today
                   then nextDate <$> today
                   else today
    tzo          = localOffset -- TODO: push tzo into state monad
    todayTZ      = localTimeGetTimezone today -- TODO: consider removing all the "todayTZ" instances
    (_, endDate) = weekOf $ localTimeUnwrap dateToStart
    endDateEOD   = flip HG.DateTime (HG.TimeOfDay 23 59 59 0) <$> endDate
    start        = getInternTime' <$> Start today
    end          = getInternTime' <$> localTime todayTZ <$> endDateEOD
  sched     <- loadShifts
  rawEvents <- getEventsForCal calId (start, end)
  events    <- validateGCalEvent' `traverse` rawEvents
  let gaps = getMinGapsInRange tzo minGapDuration sched (start, end) events
  sendSmsToRole WeeklyRole contacts $ ((mainName <> " Weekly Schedule Notice:\n") <>) $ renderGaps gaps
  where
    isEndOfWeek = (== weekStart) . HG.getWeekDay . nextDate . localTimeUnwrap

annexGapsThisWeekAlert :: IO ()
annexGapsThisWeekAlert = do
  loadEnv
  annexName     <- getAnnexName
  eventCalId    <- getAnnexEventsCalId
  staffingCalId <- getAnnexStaffingCalId
  today         <- getToday
  contacts      <- onlyActive (getInternTime' today) <$> loadContacts
  let
    dateToStart =
      if isEndOfWeek today
      then nextDate <$> today
      else today
    tzo          = localOffset -- TODO: push tzo into state monad
    todayTZ      = localTimeGetTimezone today -- TODO: consider removing all the "todayTZ" instances
    (_, endDate) = weekOf $ localTimeUnwrap dateToStart
    endDateEOD   = flip HG.DateTime (HG.TimeOfDay 23 59 59 0) <$> endDate
    start        = getInternTime' <$> Start today
    end          = getInternTime' <$> localTime todayTZ <$> endDateEOD
  rawEvents   <- (fmap.fmap) StoreEvent $ getEventsForCal eventCalId (start, end)
  events      <- (traverse . traverse) validateGCalEvent' rawEvents
  rawCoverage <- (fmap.fmap) Covered $ getEventsForCal staffingCalId (start, end)
  coverage    <- (traverse . traverse) validateGCalEvent' rawCoverage
  let gaps = getAllGapsAllDay tzo coverage events
  sendSmsToRole AnnexRole contacts $ ((annexName <> " Schedule:\n") <>) $ renderGaps gaps
  where
    isEndOfWeek = (== weekStart) . HG.getWeekDay . nextDate . localTimeUnwrap

annexEmergencySMS :: IO ()
annexEmergencySMS = do
  loadEnv
  annexName     <- getAnnexName
  eventCalId    <- getAnnexEventsCalId
  staffingCalId <- getAnnexStaffingCalId
  today         <- getInternTime' <$> getToday
  tomorrow      <- getTomorrow
  contacts      <- onlyActive today <$> loadContacts
  let tomorrowEOD = flip HG.DateTime (HG.TimeOfDay 23 59 59 0) <$> tomorrow
      start       = Start $ getInternTime' tomorrow
      end         = End $ getInternTime' tomorrowEOD
      tzo         = localOffset
  rawEvents   <- (fmap.fmap) StoreEvent $ getEventsForCal eventCalId (start, end)
  events      <- (traverse . traverse) validateGCalEvent' rawEvents
  rawCoverage <- (fmap.fmap) Covered $ getEventsForCal staffingCalId (start, end)
  coverage    <- (traverse . traverse) validateGCalEvent' rawCoverage
  let gaps = getAllGapsAllDay tzo coverage events
  if not . null . runGaps $ gaps
    then let preMsg = annexName <> " events not staffed tomorrow!:\n"
             msg = renderGaps gaps
         in
           (print (annexName <> ": sending emergency alert to: " <> (pack . show $ contacts)))
            >> (sendSmsToRole AnnexRole contacts $ preMsg <> msg)
    else print (annexName <> ": no gaps tomorrow" :: Text)


-- TODO: extract and test more of this logic
testNagAlert :: Text -> IO ()
testNagAlert staffer = do
  loadEnv
  calId <- getMainLocCalId
  today <- getToday
  let
    todayTZ      = localTimeGetTimezone today
    tzo          = localOffset
    (_, endDate) = weekOf $ localTimeUnwrap today
    endDateEOD   = flip HG.DateTime (HG.TimeOfDay 23 59 59 0) <$> endDate
    start        = getInternTime' <$> Start today
    end          = getInternTime' <$> localTime todayTZ <$> endDateEOD
  sched     <- loadShifts
  rawEvents <- getEventsForCal calId (start, end)
  events    <- validateGCalEvent' `traverse` rawEvents
  if stafferOnCal staffer events
    then print ("What a responsible bastard!" :: Text)
    else let gaps   = getMinGapsInRange tzo minGapDuration sched (start, end) events
             gapMsg = pack $ show $ renderGaps gaps
         in print $ "couldn't find " <> staffer <> pack " on cal\n" <> gapMsg

whomToNag :: [GCalEventI] -> [Active Contact] -> [Active Contact]
whomToNag es = filter $ not . flip stafferOnCal es . contactName . runActive

getMainLocName :: IO Text
getMainLocName = pack <$> getEnv "MAIN_LOC_NAME"

getMainLocCalId :: IO (CalId String)
getMainLocCalId = CalId <$> getEnv "MAIN_LOC_STAFFING_GCAL_ID"

getAnnexEventsCalId :: IO (CalId String)
getAnnexEventsCalId = CalId <$> getEnv "ANNEX_EVENTS_GCAL_ID"

getAnnexName :: IO Text
getAnnexName = pack <$> getEnv "ANNEX_NAME"

getAnnexStaffingCalId :: IO (CalId String)
getAnnexStaffingCalId = CalId <$> getEnv "ANNEX_STAFFING_GCAL_ID"

onlyActive :: InternTime -> [Contact] -> [Active Contact]
onlyActive now =  fmap Active . filter (pastSuspDate . contactSuspendUntil)
  where
    pastSuspDate Nothing = True
    pastSuspDate (Just suspDate) | suspDate <= now = True
                                 | otherwise = False

renderGaps :: Gaps [Period InternTime] -> Text
renderGaps = toText . (fmap . fmap . fmap) internToLocal

-- TODO: remove this (it was just here for debugging)
-- TODO: Replace with test of Pretty instace for gaps
displayGaps :: Gaps [Period (DisplayTZ (LocalTime Elapsed))] -> Text
displayGaps = toText

toText :: Pretty a => a -> Text
toText = textRender . pretty

configDir :: IO String
configDir = loadEnv >> getEnv "CONFIG_DIR"

textRender = renderStrict . layoutPretty defaultLayoutOptions

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

sendSmsToRole :: ContactRole -> [Active Contact] -> Text -> IO ()
sendSmsToRole role cs msg =
  sendSmsTo (filter (Set.member role . contactRoles . runActive) cs) msg

sendSms :: Text -> IO ()
sendSms msg = do
  loadEnv
  to <- pack <$> getEnv "TO_NUMBER"
  from <- pack <$> getEnv "FROM_NUMBER"
  runTwilio' (getEnv "TWILIO_ACCT_SID") (getEnv "TWILIO_AUTH_TOKEN") $ do
    resp <- TWIL.post $ PostMessage to from msg Nothing
    liftIO $ print resp

stafferOnCal :: Foldable f => Text -> f (GCalEvent a) -> Bool
stafferOnCal staffer = any (maybe False hasName . gCalSummary)
  where
    hasName = contains $ nameParser staffer

contains :: ParsecT Text () Identity [String] -> Text -> Bool
contains p input = toBool $ parse p "contains" (toLower input)
  where
    toBool = either (const False) (const True)


nameParser :: Text -> ParsecT Text u Identity [String]
nameParser name = traverse (\part -> manyTill anyChar $ string part) nameParts
  where
    nameParts = words . unpack . toLower $ name
