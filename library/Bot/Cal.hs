{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fwarn-unused-do-bind #-}

module Bot.Cal where

import Data.List (groupBy)
import Data.Text (Text)
import Bot.Time (
  PeriodIntern(..)
  , InternTime(..)
  , Period(..)
  , Date(..)
  , ShiftWeek(..)
  , TimezoneOffset(..)
  , TimeOfDay
  , ISO8601
  , Minutes
  , Covered(..)
  , Days(..)
  , StartTime
  , Start(..)
  , End(..)
  , EndTime
  , StartDate
  , EndDate
  , ShiftWeekTime
  , ShiftWeekRaw
  , RawShiftTime
  , ShiftTimeOfDay
  -- , WeekDay(..)
  , internToDate
  , periodDuration
  , periodTimeToElapsed
  )
import qualified Data.Hourglass as HG
import Data.Int (Int64)
import qualified Data.Map as Map

-- todo: refactor to use Period
data GCalEvent a = GCalEvent
  { gCalSummary :: Maybe Text
  , gCalDesc :: Maybe Text
  , gCalStart :: a
  , gCalEnd :: a
  } deriving (Show, Eq, Functor)

newtype StoreEvent a = StoreEvent { runStoreEvent :: a } deriving (Functor, Foldable, Traversable, Show)

newtype CalId a = CalId a

type RawGCalEvent = GCalEvent (ISO8601 Text)
data ResponseErrorGCalEvents = ResponseErrorGCalEvents String
type GCalEventI = GCalEvent InternTime

data GapRelationship =
    PostGap
  | PriorGap
  | SubsetGap
  | StrictSupersetGap
  | IntersectGap
  deriving (Show, Eq)

data GAuthTokenException = GAuthTokenException

mkGCalEvent :: PeriodIntern -> Text -> GCalEventI
mkGCalEvent p summ = GCalEvent (Just summ) botText (periodStart p) (periodEnd p)
  where
    botText = Just "Event created by shift bot."

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
getAllGapsAllDay _ coverage events =
  Gaps $ dayGetGaps storeEvents coveragePeriods
  where
    storeEvents = periodize . runStoreEvent <$> events
    coveragePeriods = runCovered <$> coverage

-- TODO: Generalize this logic, as we're now using it in more places than just
-- shifts (from a schedule) and events (meaning covered shifts)
-- maybe use some newtypes related to openings and coverage
dayGetGaps :: [Period InternTime] -> [GCalEventI] -> [Period InternTime]
dayGetGaps shifts events = foldr alterGaps shifts events

pairShiftsAndEvents :: [(Date, [PeriodIntern])] -> Map.Map Date [GCalEventI] -> [([PeriodIntern], [GCalEventI])]
pairShiftsAndEvents [] _ = []
pairShiftsAndEvents (d:ds) e =
  case Map.lookup (fst d) e of
    Just events -> (snd d, events) : pairShiftsAndEvents ds e
    Nothing     -> (snd d, []) : pairShiftsAndEvents ds e

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
      <$> pickShiftSelector (HG.getWeekDay date) s

gapRel :: PeriodIntern -> GCalEventI -> GapRelationship
gapRel g e
  | periodEnd g <= gCalStart e = PriorGap
  | periodStart g >= gCalEnd e = PostGap
  | periodStart g >= gCalStart e && periodEnd g <= gCalEnd e = SubsetGap
  | periodStart g < gCalStart e && periodEnd g > gCalEnd e = StrictSupersetGap
  | otherwise = IntersectGap

pickShiftSelector :: HG.WeekDay -> (ShiftWeek a -> [Period a])
pickShiftSelector HG.Monday    = wkMon
pickShiftSelector HG.Tuesday   = wkTue
pickShiftSelector HG.Wednesday = wkWed
pickShiftSelector HG.Thursday  = wkThu
pickShiftSelector HG.Friday    = wkFri
pickShiftSelector HG.Saturday  = wkSat
pickShiftSelector HG.Sunday    = wkSun

datesFromTo :: StartDate -> EndDate -> [Date]
datesFromTo (Start d1) (End d2)
  | d1 < d2   = d1 : datesFromTo (Start . nextDate $ d1) (End d2)
  | otherwise = [d2]

advanceDate :: Days Int -> Date -> Date
advanceDate (Days n) =
  flip HG.dateAddPeriod day
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

periodize :: GCalEventI -> PeriodIntern
periodize e = Period (gCalStart e) (gCalEnd e) Nothing
