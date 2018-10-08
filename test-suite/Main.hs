{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Test.Tasty
import Test.Tasty.Hspec
import Data.Aeson (decode)
import Data.Yaml (decodeEither')
import Data.Yaml.Internal
import Text.RawString.QQ
import Data.Hourglass hiding (Period)

import Example

main :: IO ()
main = do
    test <- testSpec "scheduler-fp" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  describe "parsing" $ do
    it "GCalEvent" $ do
      let json = "{\"summary\": \"this is the summary\", \"description\": \"good description\", \"start\": {\"dateTime\": \"start datetime\"}, \"end\": {\"dateTime\": \"end datetime\"}}"
          expected = (GCalEvent {gCalSummary = "this is the summary", gCalDesc = Just "good description", gCalStart = ISO8601 ("start datetime" :: String), gCalEnd = ISO8601 ("end datetime" :: String)})
          decoded = decode json
      decoded `shouldBe` Just expected

    it "ShiftWeek Shifttime" $ do
      let input = [r|
monday:
        - ["10:30", "16:00", "first"]
        - ["16:00", "21:00", "second"]
tuesday:
        - ["10:30", "16:00", "third"]
wednesday:
        - ["6:30", "16:00"]
thursday:
        - ["10:30", "16:00"]
friday:
        - ["10:30", "16:00"]
saturday: []
sunday:
        - ["10:30", "16:00"]
|]
          expected = Right $ ShiftW
            {
              wkMon =
              [
                Period {
                  periodName = Just "first"
                  , periodStart = ShiftTime (10, 30)
                  , periodEnd = ShiftTime (16, 00)
                  }
              , Period {
                  periodName = Just "second"
                  , periodStart = ShiftTime (16, 00)
                  , periodEnd = ShiftTime (21, 00)
                  }

              ]
            , wkTue =
              [
                Period {
                  periodName = Just "third"
                  , periodStart = ShiftTime (10, 30)
                  , periodEnd = ShiftTime (16, 00)
                  }
              ]
            , wkWed =
              [
                Period {
                  periodName = Nothing
                  , periodStart = ShiftTime (6, 30)
                  , periodEnd = ShiftTime (16, 00)
                  }
              ]
            , wkThu =
              [
                Period {
                  periodName = Nothing
                  , periodStart = ShiftTime (10, 30)
                  , periodEnd = ShiftTime (16, 00)
                  }
              ]
            , wkFri =
              [
                Period {
                  periodName = Nothing
                  , periodStart = ShiftTime (10, 30)
                  , periodEnd = ShiftTime (16, 00)
                  }
              ]
            , wkSat = []
            , wkSun =
              [
                Period {
                  periodName = Nothing
                  , periodStart = ShiftTime (10, 30)
                  , periodEnd = ShiftTime (16, 00)
                  }
              ]
            }

          decoded = decodeEither' input :: Either ParseException (ShiftWeek RawShiftTime)
      decoded `shouldBe` expected

  describe "groupByDate" $ do
    it "groups events by day" $ do
      let firstDay = Date {dateYear = 2018, dateMonth = October, dateDay = 8}
          secondDay = Date {dateYear = 2018, dateMonth = October, dateDay = 9}
          firstTime = TimeOfDay {todHour = 0, todMin = 41, todSec = 5, todNSec = 0}
          secondTime = TimeOfDay {todHour = 2, todMin = 41, todSec = 5, todNSec = 0}
          thirdTime = TimeOfDay {todHour = 4, todMin = 41, todSec = 5, todNSec = 0}
          fourthTime = TimeOfDay {todHour = 6, todMin = 41, todSec = 5, todNSec = 0}
          p1Start = timeGetElapsed $ DateTime firstDay firstTime
          p1End = timeGetElapsed $ DateTime firstDay secondTime
          p2Start = timeGetElapsed $ DateTime firstDay thirdTime
          p2End = timeGetElapsed $ DateTime firstDay fourthTime
          p3Start = timeGetElapsed $ DateTime secondDay firstTime
          p3End = timeGetElapsed $ DateTime secondDay secondTime
          e1 = GCalEvent "first event" Nothing p1Start p1End
          e2 = GCalEvent "second event" Nothing p2Start p2End
          e3 = GCalEvent "third event" Nothing p3Start p3End

      groupByDate [e1, e2, e3] `shouldBe` [ (firstDay, [e1, e2])
                                          , (secondDay, [e3])
                                          ]

  describe "shrinkGap" $ do
    context "with a pior partially overlapping gap" $ do
      it "creates a gap that's the size of the gap not covered by the event" $ do
        let baseDate = Date 01 January 1999
            gapStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 0 0 0 0)
            gapEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 5 0 0 0)
            gap = Period gapStart gapEnd Nothing
            eventStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 1 30 0 0)
            eventEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 7 0 0 0)
            event = GCalEvent "event" Nothing eventStart eventEnd

            expectedGap = Period gapStart eventStart Nothing

        shrinkGap gap event `shouldBe` expectedGap

    context "with a post partially overlapping gap" $ do
      it "creates a gap that's the size of the gap not covered by the event" $ do
        let baseDate = Date 01 January 1999
            gapStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 2 0 0 0)
            gapEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 5 0 0 0)
            gap = Period gapStart gapEnd Nothing
            eventStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 0 0 0 0)
            eventEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 3 0 0 0)
            event = GCalEvent "event" Nothing eventStart eventEnd

            expectedGap = Period eventEnd gapEnd Nothing

        shrinkGap gap event `shouldBe` expectedGap

    describe "gapRel" $ do
      context "with a gap prior to an event" $ do
        it "is PriorGap" $ do
          let baseDate = Date 01 January 1999
              gapStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 4 0 0 0)
              gapEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 8 0 0 0)
              gap = Period gapStart gapEnd Nothing
              eventStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 13 0 0 0)
              eventEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 15 0 0 0)
              event = GCalEvent "event" Nothing eventStart eventEnd

          gapRel gap event `shouldBe` PriorGap

      context "with a gap after to an event" $ do
        it "is PostGap" $ do
          let baseDate = Date 01 January 1999
              gapStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 4 0 0 0)
              gapEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 8 0 0 0)
              gap = Period gapStart gapEnd Nothing
              eventStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 0 0 0 0)
              eventEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 3 0 0 0)
              event = GCalEvent "event" Nothing eventStart eventEnd

          gapRel gap event `shouldBe` PostGap

      context "with a gap that is a strict-subset of an event" $ do
        it "is SubsetGap" $ do
          let baseDate = Date 01 January 1999
              gapStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 4 0 0 0)
              gapEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 8 0 0 0)
              gap = Period gapStart gapEnd Nothing
              eventStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 3 0 0 0)
              eventEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 9 0 0 0)
              event = GCalEvent "event" Nothing eventStart eventEnd

          gapRel gap event `shouldBe` SubsetGap

      context "with a gap that is equal to an event" $ do
        it "is SubsetGap" $ do
          let baseDate = Date 01 January 1999
              gapStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 4 0 0 0)
              gapEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 8 0 0 0)
              gap = Period gapStart gapEnd Nothing
              eventStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 4 0 0 0)
              eventEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 8 0 0 0)
              event = GCalEvent "event" Nothing eventStart eventEnd

          gapRel gap event `shouldBe` SubsetGap

      context "with a gap that is a strict-superset of an event" $ do
        it "is StrictSupersetGap" $ do
          let baseDate = Date 01 January 1999
              gapStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 2 0 0 0)
              gapEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 10 0 0 0)
              gap = Period gapStart gapEnd Nothing
              eventStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 3 0 0 0)
              eventEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 9 0 0 0)
              event = GCalEvent "event" Nothing eventStart eventEnd

          gapRel gap event `shouldBe` StrictSupersetGap

--       describe "intersectGap" $ do
--         context "with a prior gap that intersects an event" $ do
--           it "is an IntersectGap" $ do
--             let baseDate = Date 01 January 1999
--                 gapStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 4 0 0 0)
--                 gapEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 8 0 0 0)
--                 gap = Period gapStart gapEnd Nothing
--                 eventStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 5 0 0 0)
--                 eventEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 10 0 0 0)
--                 event = GCalEvent "event" Nothing eventStart eventEnd

--             gapRel gap event `shouldBe` IntersectGap

--         context "with a prior gap and equal ending" $ do
--           it "is an intersectGap" $ do
--             let baseDate = Date {dateYear = 2018, dateMonth = March, dateDay = 4}
--                 gapStart = timeGetElapsed $ DateTime baseDate $ TimeOfDay 10 30 0 0
--                 gapEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 16 0 0 0)
--                 gap = Period {periodStart = gapStart, periodEnd = gapEnd, periodName = Nothing}

--                 eventStart = timeGetElapsed $ DateTime baseDate $ TimeOfDay 12 0 0 0
--                 eventEnd = timeGetElapsed $ DateTime baseDate $ TimeOfDay 16 0 0 0
--                 event = GCalEvent {gCalSummary = "event 2", gCalDesc = Nothing, gCalStart = eventStart, gCalEnd = eventEnd}

--             gapRel gap event `shouldBe` IntersectGap

--         context "with a post gap that intersects an event" $ do
--           it "is an IntersectGap" $ do
--             let baseDate = Date 01 January 1999
--                 gapStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 7 0 0 0)
--                 gapEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 12 0 0 0)
--                 gap = Period gapStart gapEnd Nothing
--                 eventStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 5 0 0 0)
--                 eventEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 10 0 0 0)
--                 event = GCalEvent "event" Nothing eventStart eventEnd

--             gapRel gap event `shouldBe` IntersectGap

--         context "with a post gap and equal start" $ do
--           it "is an IntersectGap" $ do
--             let baseDate = Date 01 January 1999
--                 gapStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 7 0 0 0)
--                 gapEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 12 0 0 0)
--                 gap = Period gapStart gapEnd Nothing
--                 eventStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 7 0 0 0)
--                 eventEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 10 0 0 0)
--                 event = GCalEvent "event" Nothing eventStart eventEnd

--             gapRel gap event `shouldBe` IntersectGap

    describe "splitGap" $ do
      it "splits a gap by a (strict-subset) event" $ do
        let baseDate = Date 01 January 1999
            gapStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 1 0 0 0)
            gapEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 12 0 0 0)
            gap = Period gapStart gapEnd Nothing
            eventStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 5 0 0 0)
            eventEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 10 0 0 0)
            event = GCalEvent "event" Nothing eventStart eventEnd

            expectedFirst = Period gapStart eventStart Nothing
            expectedSecond = Period eventEnd gapEnd Nothing

        splitGap gap event `shouldBe` [expectedFirst, expectedSecond]

    describe "alterGaps" $ do
      context "with an event that's in-between gaps" $ do
        it "doesn't alter the gaps" $ do
          let baseDate = Date { dateDay = 01, dateMonth = January, dateYear = 1999 }
              gap1Start = timeGetElapsed $ DateTime baseDate (TimeOfDay 1 0 0 0)
              gap1End = timeGetElapsed $ DateTime baseDate (TimeOfDay 3 0 0 0)
              gap1 = Period gap1Start gap1End (Just "gap 1")
              gap2Start = timeGetElapsed $ DateTime baseDate (TimeOfDay 9 0 0 0)
              gap2End = timeGetElapsed $ DateTime baseDate (TimeOfDay 12 0 0 0)
              gap2 = Period gap2Start gap2End (Just "gap 2")

              eventStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 4 0 0 0)
              eventEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 8 0 0 0)
              event = GCalEvent "event" Nothing eventStart eventEnd

          alterGaps [gap1, gap2] event `shouldBe` [gap1, gap2]

      context "with an event that covers one of the gaps" $ do
        it "removes that gap" $ do
          let baseDate = Date 01 January 1999
              gap1Start = timeGetElapsed $ DateTime baseDate (TimeOfDay 1 0 0 0)
              gap1End = timeGetElapsed $ DateTime baseDate (TimeOfDay 3 0 0 0)
              gap1 = Period gap1Start gap1End (Just "gap 1")
              gap2Start = timeGetElapsed $ DateTime baseDate (TimeOfDay 9 0 0 0)
              gap2End = timeGetElapsed $ DateTime baseDate (TimeOfDay 12 0 0 0)
              gap2 = Period gap2Start gap2End (Just "gap 2")

              eventStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 9 0 0 0)
              eventEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 14 0 0 0)
              event = GCalEvent "event" Nothing eventStart eventEnd

          alterGaps [gap1, gap2] event `shouldBe` [gap1]

      context "with an event that partially covers one of the gaps" $ do
        it "removes that gap" $ do
          let baseDate = Date { dateDay = 01, dateMonth = January, dateYear = 1999 }

              gap1Start = timeGetElapsed $ DateTime baseDate (TimeOfDay 1 0 0 0)
              gap1End = timeGetElapsed $ DateTime baseDate (TimeOfDay 3 0 0 0)
              gap1 = Period gap1Start gap1End (Just "gap 1")

              gap2Start = timeGetElapsed $ DateTime baseDate (TimeOfDay 9 0 0 0)
              gap2End = timeGetElapsed $ DateTime baseDate (TimeOfDay 12 0 0 0)
              gap2 = Period gap2Start gap2End (Just "gap 2")

              eventStart = timeGetElapsed $ DateTime baseDate (TimeOfDay 10 20 0 0)
              eventEnd = timeGetElapsed $ DateTime baseDate (TimeOfDay 13 0 0 0)
              event = GCalEvent "event" Nothing eventStart eventEnd

          alterGaps [gap1, gap2] event `shouldBe` [gap1, Period gap2Start eventStart Nothing]

    describe "getGapsInRange" $ do
      context "with a single gap" $ do
        it "finds the gap" $ do
          let shifts = ShiftW
                {
                  wkMon = []
                , wkTue = []
                , wkWed = []
                , wkThu = []
                , wkFri = []
                , wkSat = []
                , wkSun =
                  [
                    Period {
                      periodName = Nothing
                      , periodStart = ShiftTime (TimeOfDay (Hours 10) (Minutes 30) (Seconds 0) (NanoSeconds 0))
                      , periodEnd = ShiftTime (TimeOfDay (Hours 16) (Minutes 00) (Seconds 0) (NanoSeconds 0))
                      }
                  ]
                }

              sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }

              event1Start = timeGetElapsed $ DateTime sunday (TimeOfDay 10 30 0 0)
              event1End = timeGetElapsed $ DateTime sunday (TimeOfDay 12 0 0 0)
              event1 = GCalEvent "event 1" Nothing event1Start event1End

              event2Start = timeGetElapsed $ DateTime sunday (TimeOfDay 12 0 0 0)
              event2End = timeGetElapsed $ DateTime sunday (TimeOfDay 14 0 0 0)
              event2 = GCalEvent "event 2" Nothing event2Start event2End

              events = [event1, event2]

              gapStart = timeGetElapsed $ DateTime sunday (TimeOfDay 14 0 0 0)
              gapEnd = timeGetElapsed $ DateTime sunday (TimeOfDay 16 0 0 0)
              expectedGap = Period gapStart gapEnd Nothing

              dateRange = (sunday, sunday)

              results = getGapsInRange shifts dateRange events

          results `shouldBe` Gaps [expectedGap]

      context "with gaps on different days" $ do
        it "finds the gap" $ do
          let
            shifts = ShiftW
                {
                  wkMon = []
                , wkTue = []
                , wkWed = []
                , wkThu = []
                , wkSat = []
                , wkFri =
                  [
                    Period {
                      periodName = Nothing
                      , periodStart = ShiftTime (TimeOfDay (Hours 10) (Minutes 30) (Seconds 0) (NanoSeconds 0))
                      , periodEnd = ShiftTime (TimeOfDay (Hours 16) (Minutes 00) (Seconds 0) (NanoSeconds 0))
                      }
                  ]
                , wkSun =
                  [
                    Period {
                      periodName = Nothing
                      , periodStart = ShiftTime (TimeOfDay (Hours 10) (Minutes 30) (Seconds 0) (NanoSeconds 0))
                      , periodEnd = ShiftTime (TimeOfDay (Hours 16) (Minutes 00) (Seconds 0) (NanoSeconds 0))
                      }
                  ]
                }

            sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }

            event1Start = timeGetElapsed $ DateTime sunday (TimeOfDay 10 30 0 0)
            event1End = timeGetElapsed $ DateTime sunday (TimeOfDay 12 0 0 0)
            event1 = GCalEvent "event 1" Nothing event1Start event1End

            event2Start = timeGetElapsed $ DateTime sunday (TimeOfDay 12 0 0 0)
            event2End = timeGetElapsed $ DateTime sunday (TimeOfDay 14 0 0 0)
            event2 = GCalEvent "event 2" Nothing event2Start event2End

            sundayEvents = [event1, event2]

            gap1Start = timeGetElapsed $ DateTime friday (TimeOfDay 14 0 0 0)
            gap1End = timeGetElapsed $ DateTime friday (TimeOfDay 16 0 0 0)
            expectedGap1 = Period gap1Start gap1End Nothing

            friday = Date { dateDay = 02, dateMonth = March, dateYear = 2018 }

            event3Start = timeGetElapsed $ DateTime friday (TimeOfDay 10 30 0 0)
            event3End = timeGetElapsed $ DateTime friday (TimeOfDay 12 0 0 0)
            event3 = GCalEvent "event 3" Nothing event3Start event3End

            event4Start = timeGetElapsed $ DateTime friday (TimeOfDay 12 0 0 0)
            event4End = timeGetElapsed $ DateTime friday (TimeOfDay 14 0 0 0)
            event4 = GCalEvent "event 4" Nothing event4Start event4End

            fridayEvents = [event3, event4]

            gap2Start = timeGetElapsed $ DateTime sunday (TimeOfDay 14 0 0 0)
            gap2End = timeGetElapsed $ DateTime sunday (TimeOfDay 16 0 0 0)
            expectedGap2 = Period gap2Start gap2End Nothing

            dateRange = (friday, sunday)

            results = getGapsInRange shifts dateRange (fridayEvents ++ sundayEvents)

          results `shouldBe` Gaps [expectedGap1, expectedGap2]

      context "with full coverage of schedule" $ do
        it "finds no gaps" $ do
          let
            shifts = ShiftW
                {
                  wkMon = []
                , wkTue = []
                , wkWed = []
                , wkThu = []
                , wkFri = []
                , wkSat = []
                , wkSun =
                  [
                    Period {
                      periodName = Nothing
                      , periodStart = ShiftTime (TimeOfDay (Hours 10) (Minutes 30) (Seconds 0) (NanoSeconds 0))
                      , periodEnd = ShiftTime (TimeOfDay (Hours 16) (Minutes 00) (Seconds 0) (NanoSeconds 0))
                      }
                  ]
                }

            sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }

            event1Start = timeGetElapsed $ DateTime sunday (TimeOfDay 10 30 0 0)
            event1End = timeGetElapsed $ DateTime sunday (TimeOfDay 12 0 0 0)
            event1 = GCalEvent "event 1" Nothing event1Start event1End

            event2Start = timeGetElapsed $ DateTime sunday (TimeOfDay 12 0 0 0)
            event2End = timeGetElapsed $ DateTime sunday (TimeOfDay 16 0 0 0)
            event2 = GCalEvent "event 2" Nothing event2Start event2End

            sundayEvents = [event1, event2]
            dateRange = ( Date { dateDay = 02, dateMonth = March, dateYear = 2018 }
                        , Date { dateDay = 06, dateMonth = March, dateYear = 2018 }
                        )

            results = getGapsInRange shifts dateRange sundayEvents

          results `shouldBe` Gaps []

      context "with an empty schedule" $ do
        it "finds no gaps" $ do
          let
            shifts = ShiftW
                {
                  wkMon = []
                , wkTue = []
                , wkWed = []
                , wkThu = []
                , wkFri = []
                , wkSat = []
                , wkSun = []
                }

            sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }

            event1Start = timeGetElapsed $ DateTime sunday (TimeOfDay 10 30 0 0)
            event1End = timeGetElapsed $ DateTime sunday (TimeOfDay 12 0 0 0)
            event1 = GCalEvent "event 1" Nothing event1Start event1End

            sundayEvents = [ event1 ]

            dateRange = ( Date { dateDay = 01, dateMonth = March, dateYear = 2018 }
                        , Date { dateDay = 10, dateMonth = March, dateYear = 2018 }
                        )
            results = getGapsInRange shifts dateRange sundayEvents

          results `shouldBe` Gaps []

      context "with no events" $ do
        it "turns schedule shifts into gaps" $ do
          let
            sundayShift = Period
              {
                periodName = Just "sunday shift"
              , periodStart = ShiftTime (TimeOfDay (Hours 10) (Minutes 30) (Seconds 0) (NanoSeconds 0))
              , periodEnd = ShiftTime (TimeOfDay (Hours 16) (Minutes 00) (Seconds 0) (NanoSeconds 0))
              }
            shifts = ShiftW
                {
                  wkMon = []
                , wkTue = []
                , wkWed = []
                , wkThu = []
                , wkFri = []
                , wkSat = []
                , wkSun = [ sundayShift ]
                }

            sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }
            gapStart = timeGetElapsed $ DateTime sunday (TimeOfDay 10 30 0 0)
            gapEnd = timeGetElapsed $ DateTime sunday (TimeOfDay 16 0 0 0)
            expectedGap = Period gapStart gapEnd $ Just "sunday shift"

            results = getGapsInRange  shifts (sunday, sunday) []

          results `shouldBe` Gaps [expectedGap]


-- helpers

instance Eq Data.Yaml.Internal.ParseException where
  _ == _ = False
