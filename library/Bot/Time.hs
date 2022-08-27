{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Bot.Time where

import Control.Exception (Exception, throwIO)
import Formatting (format, (%))
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

import qualified Formatting.Formatters as FMT
import Prettyprinter (Pretty(..), defaultLayoutOptions, layoutPretty, vsep, (<+>))
import Data.Text (Text(..), pack, unpack)
import Time.System (localDateCurrentAt)
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
                      , localTimeGetTimezone
                      , localTime
                      , localTimeUnwrap
                      , timezoneOffset
                      , dateAddPeriod
                      , timeGetDateTimeOfDay
                      , getWeekDay
                      , timeDiff
                      , toSeconds
                      , fromSeconds
                      , timeGetDate
                      )
import qualified Data.Hourglass as HG
import Data.Int (Int64)
import qualified Data.Map as Map

data ISO8601Error = ISO8601Error String

type IxGaps a = Map.Map Int (Period a)
newtype DisplayMonthDate a = DisplayMonthDate a
newtype Concise a = Concise a
newtype Gaps a = Gaps { runGaps :: a } deriving (Show, Eq, Functor)

-- track that number is a date for display
newtype DisplayDate a = DisplayDate { runDD :: a } deriving (Functor)


data ShiftTime a =
  ShiftTime { stTime :: a
            , stZone :: HG.TimezoneOffset
            }
  deriving (Show, Eq, Functor)

newtype Start a = Start { runStart :: a } deriving (Functor, Show, Eq)
newtype End a = End { runEnd :: a } deriving (Functor, Show, Eq)
type StartTime = Start InternTime
type EndTime = End InternTime
type StartDate = Start HG.Date
type EndDate = End HG.Date
type ShiftWeekTime = ShiftWeek ShiftTimeOfDay
type ShiftWeekRaw = ShiftWeek RawShiftTime
type RawShiftTime = ShiftTime (Int64, Int64)
type ShiftTimeOfDay = ShiftTime HG.TimeOfDay
newtype ISO8601 a = ISO8601 a deriving (Show, Eq)
type TimeOfDay = HG.TimeOfDay
type Hours = HG.Hours
type Minutes = HG.Minutes
type Elapsed = HG.Elapsed
type LocalTime = HG.LocalTime
-- type WeekDay = HG.WeekDay
type TimezoneOffset = HG.TimezoneOffset
type Date = HG.Date
type InternTime = Intern (HG.LocalTime HG.Elapsed)
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


-- ensure that we convert to correct TZ for display purposes
newtype DisplayTZ a = DisplayTZ { runDTZ :: a } deriving (Functor, Eq, Show)
newtype Days a = Days a deriving (Show, Eq)
newtype Intern a = Intern { runIntern :: a } deriving (Show, Functor, Ord, Eq)
-- ensure that internal time is always UTC
newtype Covered a = Covered { runCovered :: a } deriving (Functor, Foldable, Traversable, Show)

data Period a = Period { periodStart :: a
                       , periodEnd :: a
                       , periodName :: Maybe Text
                       } deriving (Show, Eq, Functor)

type PeriodIntern = Period InternTime

localTimeGetTimezone = Data.Hourglass.localTimeGetTimezone
-- localtime = Data.Hourglass.localTime
localTimeUnwrap = Data.Hourglass.localTimeUnwrap

getCurrentTime :: IO InternTime
getCurrentTime = getInternTime' <$> localDateCurrentAt utcOffset

localOffset :: Bot.Time.TimezoneOffset
localOffset = TimezoneOffset (-300)

getInternTime :: Timeable t => Bot.Time.TimezoneOffset -> t -> InternTime
getInternTime currentTZ =
  Intern
  . HG.localTimeSetTimezone utcOffset
  . HG.localTime currentTZ
  . HG.timeGetElapsed

getInternTime' :: Timeable t => Bot.Time.LocalTime t -> InternTime
getInternTime' lt =
  getInternTime (HG.localTimeGetTimezone lt) $ HG.localTimeUnwrap lt

internTimeFromUTC :: Timeable t => t -> InternTime
internTimeFromUTC = getInternTime utcOffset

internTimeFromLocalOffset :: Timeable t => t -> InternTime
internTimeFromLocalOffset = getInternTime localOffset

utcOffset :: Bot.Time.TimezoneOffset
utcOffset = TimezoneOffset $ timezoneOffset UTC

displayTZ :: (HG.Time a) => Bot.Time.TimezoneOffset -> Bot.Time.LocalTime a -> DisplayTZ (Bot.Time.LocalTime a)
displayTZ tz = DisplayTZ . HG.localTimeSetTimezone tz

internToDisplay :: Bot.Time.TimezoneOffset -> InternTime -> DisplayTZ (Bot.Time.LocalTime HG.Elapsed)
internToDisplay tz = DisplayTZ . HG.localTimeSetTimezone tz . runIntern

internToLocal :: InternTime -> DisplayTZ (Bot.Time.LocalTime HG.Elapsed)
internToLocal = internToDisplay localOffset

-- this should eventually be in a state monad
getToday :: IO (HG.LocalTime HG.Date)
getToday = (fmap . fmap . fmap) dtDate localDateCurrentAt utcOffset

periodDuration :: PeriodIntern -> (HG.Minutes, HG.Seconds)
periodDuration a = fromSeconds $ timeDiff (inUTC . periodEnd $ a) (inUTC . periodStart $ a)
  where
    inUTC = HG.localTimeUnwrap . runIntern

internToDateTime :: HG.TimezoneOffset -> InternTime -> DateTime
internToDateTime tzo = timeGetDateTimeOfDay . HG.localTimeUnwrap . HG.localTimeSetTimezone tzo . runIntern

internToDate :: HG.TimezoneOffset -> InternTime -> HG.Date
internToDate tzo = dtDate . internToDateTime tzo

periodTimeToElapsed :: HG.Date -> Period ShiftTimeOfDay -> PeriodIntern
periodTimeToElapsed date a =
  Period
  (tODToIntern . periodStart $ a)
  (tODToIntern . periodEnd   $ a)
  (periodName a)
  where
    tODToIntern :: ShiftTime HG.TimeOfDay -> InternTime
    tODToIntern (ShiftTime t z) = getInternTime z (mkDateTime t)

    mkDateTime = DateTime date


instance FromJSON InternTime where
  parseJSON = withText "InternTime" $ \txt ->
    either (fail . show) pure $ parse8601 $ ISO8601 txt

instance Pretty (IxGaps (DisplayTZ (HG.LocalTime HG.Elapsed))) where --TODO: handle empty
  pretty im = vsep $ fmap prettyIx $ Map.toList im
    where
      prettyIx (i, p) = pretty i <> ":" <+> pretty p

instance (Pretty a) => Pretty (Gaps [a]) where
  pretty (Gaps []) = "All shifts covered this week!"
  pretty (Gaps a) = vsep $ ["Open shifts: "]
                            <> fmap pretty a

instance Pretty (Period (DisplayTZ (HG.LocalTime HG.Elapsed))) where
  pretty (Period s _ (Just name)) = pretty ((internToDate <$> HG.localTimeGetTimezone <*> Intern) <$> s) <+> pretty name
  pretty (Period s e Nothing)     = pretty ((internToDate <$> HG.localTimeGetTimezone <*> Intern) <$> s) <+> pretty s
                                    <+> "to" <+> pretty e

instance Pretty (DisplayTZ HG.Date) where
  pretty d = pretty  (getWeekDay <$> d)

instance Pretty (DisplayMonthDate (DisplayTZ (HG.LocalTime HG.Date))) where
  pretty (DisplayMonthDate d) =
    pretty (Concise $ dateMonth . HG.localTimeUnwrap <$> d)
    <+> pretty (DisplayDate . dateDay . HG.localTimeUnwrap <$> d)

instance Pretty (DisplayTZ (HG.LocalTime HG.Date)) where
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
  pretty (DisplayTZ (DisplayDate a)) = pretty $ format FMT.int a

instance Pretty (DisplayTZ (HG.LocalTime HG.Elapsed)) where
  pretty (DisplayTZ e) =
    pretty $ DisplayTZ $ toTime e
    where
      toTime = dtTime . HG.timeGetDateTimeOfDay . HG.localTimeUnwrap

  -- TODO: clean up
instance Pretty (DisplayTZ HG.TimeOfDay) where
  pretty (DisplayTZ t) = do
    let formattedTime =
          if minutes == 0
          then format (FMT.int ) hoursFmt
          else format (FMT.int % ":" % FMT.left 2 '0' ) hoursFmt minutes

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

parse8601 :: ISO8601 Text -> Either ISO8601Error InternTime
parse8601 (ISO8601 s) =
  toEither maybeIntern
  where
    toEither = maybe (Left . ISO8601Error $ unpack s) Right
    maybeIntern = internTimeFromUTC . HG.timeGetElapsed
                  <$> HG.timeParse ISO8601_DateAndTime (unpack s)

show8601 :: HG.TimezoneOffset -> InternTime -> Text
show8601 tzo = pack
  . HG.localTimePrint ISO8601_DateAndTime
  . HG.localTimeSetTimezone tzo
  . runIntern

show8601UTC :: InternTime -> Text
show8601UTC =  show8601 utcOffset

show8601Local :: InternTime -> Text
show8601Local = show8601 localOffset

instance Exception ISO8601Error

instance Show ISO8601Error where
  show (ISO8601Error str) =
    "Error: Couldn't parse ISO8601 date format from: " <> str
