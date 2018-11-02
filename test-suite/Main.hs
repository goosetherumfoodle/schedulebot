{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Test.Tasty
import Test.Tasty.Hspec
import Data.Aeson (decode)
import Data.Yaml (decodeEither')
import Data.Yaml.Internal (ParseException)
import Text.RawString.QQ (r)
import Data.Hourglass hiding (Period)
import Data.Text (Text)
import qualified Data.Set as Set

import Example

main :: IO ()
main = do
    test <- testSpec "scheduler-fp" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  describe "parsing" $ do
    describe "parseSuspendDays" $ do
      context "with valid input and a number" $ do
        it "should retrieve days" $ do
          let text = " susPend \t123 99  "

          parseSuspendDays text `shouldBe` (Just (Just (Days 123)))

      context "with valid input but no number" $ do
        it "should retrieve days" $ do
          let text = " susPend   "

          parseSuspendDays text `shouldBe` (Just Nothing)


      context "with invalid input" $ do
        it "should be nothing" $ do
          let text = "supnd 123"

          parseSuspendDays text `shouldBe` Nothing

    it "GCalEvent" $ do
      let json = "{\"summary\": \"this is the summary\",         \
                 \\"description\": \"good description\",         \
                 \\"start\": {\"dateTime\": \"start datetime\"}, \
                 \\"end\": {\"dateTime\": \"end datetime\"}}"
          expected = GCalEvent {
              gCalSummary = Just "this is the summary"
            , gCalDesc = Just "good description"
            , gCalStart = ISO8601 ("start datetime" :: Text)
            , gCalEnd = ISO8601 ("end datetime" :: Text)
            }

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
                  , periodStart = shiftTimeWZone (10, 30)
                  , periodEnd = shiftTimeWZone (16, 00)
                  }
              , Period {
                  periodName = Just "second"
                  , periodStart = shiftTimeWZone (16, 00)
                  , periodEnd = shiftTimeWZone (21, 00)
                  }

              ]
            , wkTue =
              [
                Period {
                  periodName = Just "third"
                  , periodStart = shiftTimeWZone (10, 30)
                  , periodEnd = shiftTimeWZone (16, 00)
                  }
              ]
            , wkWed =
              [
                Period {
                  periodName = Nothing
                  , periodStart = shiftTimeWZone (6, 30)
                  , periodEnd = shiftTimeWZone (16, 00)
                  }
              ]
            , wkThu =
              [
                Period {
                  periodName = Nothing
                  , periodStart = shiftTimeWZone (10, 30)
                  , periodEnd = shiftTimeWZone (16, 00)
                  }
              ]
            , wkFri =
              [
                Period {
                  periodName = Nothing
                  , periodStart = shiftTimeWZone (10, 30)
                  , periodEnd = shiftTimeWZone (16, 00)
                  }
              ]
            , wkSat = []
            , wkSun =
              [
                Period {
                  periodName = Nothing
                  , periodStart = shiftTimeWZone (10, 30)
                  , periodEnd = shiftTimeWZone (16, 00)
                  }
              ]
            }

          decoded = decodeEither' input :: Either ParseException (ShiftWeek RawShiftTime)

      decoded `shouldBe` expected

    describe "[Contact]" $ do
      it "parses contacts" $ do
        let input = [r|
- suspendUntil: Null
  name: Mark Corrigan
  id: 2
  number: '+1231231234'
  roles: ['weekly', 'annex']

- name: Big Suze
  id: 3
  number: '+13213214321'
  suspendUntil: Null
  roles: []
|]
            contacts = [
              Contact "Mark Corrigan" "+1231231234" Nothing 2 (Set.fromList [WeeklyRole, AnnexRole])
              , Contact "Big Suze" "+13213214321" Nothing 3 Set.empty
              ]

        decodeEither' input `shouldBe` Right contacts

  describe "groupByDate" $ do
    it "groups events by day" $ do
      let firstDay = Date {dateYear = 2018, dateMonth = October, dateDay = 8}
          secondDay = Date {dateYear = 2018, dateMonth = October, dateDay = 9}
          firstTime = TimeOfDay {todHour = 0, todMin = 41, todSec = 5, todNSec = 0}
          secondTime = TimeOfDay {todHour = 2, todMin = 41, todSec = 5, todNSec = 0}
          thirdTime = TimeOfDay {todHour = 4, todMin = 41, todSec = 5, todNSec = 0}
          fourthTime = TimeOfDay {todHour = 6, todMin = 41, todSec = 5, todNSec = 0}
          p1Start = internTimeFromLocalOffset $ DateTime firstDay firstTime
          p1End = internTimeFromLocalOffset $ DateTime firstDay secondTime
          p2Start = internTimeFromLocalOffset $ DateTime firstDay thirdTime
          p2End = internTimeFromLocalOffset $ DateTime firstDay fourthTime
          p3Start = internTimeFromLocalOffset $ DateTime secondDay firstTime
          p3End = internTimeFromLocalOffset $ DateTime secondDay secondTime
          e1 = GCalEvent (Just "first event") Nothing p1Start p1End
          e2 = GCalEvent (Just "second event") Nothing p2Start p2End
          e3 = GCalEvent (Just "third event") Nothing p3Start p3End

      groupByDate localOffset [e1, e2, e3] `shouldBe` [ (firstDay, [e1, e2])
                                                      , (secondDay, [e3])
                                                      ]

  describe "shrinkGap" $ do
    context "with a pior partially overlapping gap" $ do
      it "creates a gap that's the size of the gap not covered by the event" $ do
        let baseDate = Date 01 January 1999
            gapStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 0 0 0 0)
            gapEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 5 0 0 0)
            gap = Period gapStart gapEnd Nothing
            eventStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 1 30 0 0)
            eventEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 7 0 0 0)
            event = GCalEvent (Just "event") Nothing eventStart eventEnd

            expectedGap = Period gapStart eventStart Nothing

        shrinkGap gap event `shouldBe` expectedGap

    context "with a post partially overlapping gap" $ do
      it "creates a gap that's the size of the gap not covered by the event" $ do
        let baseDate = Date 01 January 1999
            gapStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 2 0 0 0)
            gapEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 5 0 0 0)
            gap = Period gapStart gapEnd Nothing
            eventStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 0 0 0 0)
            eventEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 3 0 0 0)
            event = GCalEvent (Just "event") Nothing eventStart eventEnd

            expectedGap = Period eventEnd gapEnd Nothing

        shrinkGap gap event `shouldBe` expectedGap

  describe "gapRel" $ do
    context "with a gap prior to an event" $ do
      it "is PriorGap" $ do
        let baseDate = Date 01 January 1999
            gapStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 4 0 0 0)
            gapEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 8 0 0 0)
            gap = Period gapStart gapEnd Nothing
            eventStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 13 0 0 0)
            eventEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 15 0 0 0)
            event = GCalEvent (Just "event") Nothing eventStart eventEnd

        gapRel gap event `shouldBe` PriorGap

    context "with a gap after to an event" $ do
      it "is PostGap" $ do
        let baseDate = Date 01 January 1999
            gapStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 4 0 0 0)
            gapEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 8 0 0 0)
            gap = Period gapStart gapEnd Nothing
            eventStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 0 0 0 0)
            eventEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 3 0 0 0)
            event = GCalEvent (Just "event") Nothing eventStart eventEnd

        gapRel gap event `shouldBe` PostGap

    context "with a gap that is a strict-subset of an event" $ do
      it "is SubsetGap" $ do
        let baseDate = Date 01 January 1999
            gapStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 4 0 0 0)
            gapEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 8 0 0 0)
            gap = Period gapStart gapEnd Nothing
            eventStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 3 0 0 0)
            eventEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 9 0 0 0)
            event = GCalEvent (Just "event") Nothing eventStart eventEnd

        gapRel gap event `shouldBe` SubsetGap

    context "with a gap that is equal to an event" $ do
      it "is SubsetGap" $ do
        let baseDate = Date 01 January 1999
            gapStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 4 0 0 0)
            gapEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 8 0 0 0)
            gap = Period gapStart gapEnd Nothing
            eventStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 4 0 0 0)
            eventEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 8 0 0 0)
            event = GCalEvent (Just "event") Nothing eventStart eventEnd

        gapRel gap event `shouldBe` SubsetGap

    context "with a gap that is a strict-superset of an event" $ do
      it "is StrictSupersetGap" $ do
        let baseDate = Date 01 January 1999
            gapStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 2 0 0 0)
            gapEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 10 0 0 0)
            gap = Period gapStart gapEnd Nothing
            eventStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 3 0 0 0)
            eventEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 9 0 0 0)
            event = GCalEvent (Just "event") Nothing eventStart eventEnd

        gapRel gap event `shouldBe` StrictSupersetGap

  describe "intersectGap" $ do
    context "with a prior gap that intersects an event" $ do
      it "is an IntersectGap" $ do
        let baseDate = Date 01 January 1999
            gapStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 4 0 0 0)
            gapEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 8 0 0 0)
            gap = Period gapStart gapEnd Nothing
            eventStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 5 0 0 0)
            eventEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 10 0 0 0)
            event = GCalEvent (Just "event") Nothing eventStart eventEnd

        gapRel gap event `shouldBe` IntersectGap

    context "with a prior gap and equal ending" $ do
      it "is an intersectGap" $ do
        let baseDate = Date {dateYear = 2018, dateMonth = March, dateDay = 4}
            gapStart = internTimeFromLocalOffset $ DateTime baseDate $ TimeOfDay 10 30 0 0
            gapEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 16 0 0 0)
            gap = Period {periodStart = gapStart, periodEnd = gapEnd, periodName = Nothing}

            eventStart = internTimeFromLocalOffset $ DateTime baseDate $ TimeOfDay 12 0 0 0
            eventEnd = internTimeFromLocalOffset $ DateTime baseDate $ TimeOfDay 16 0 0 0
            event = GCalEvent {gCalSummary = (Just "event 2"), gCalDesc = Nothing, gCalStart = eventStart, gCalEnd = eventEnd}

        gapRel gap event `shouldBe` IntersectGap

    context "with a post gap that intersects an event" $ do
      it "is an IntersectGap" $ do
        let baseDate = Date 01 January 1999
            gapStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 7 0 0 0)
            gapEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 12 0 0 0)
            gap = Period gapStart gapEnd Nothing
            eventStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 5 0 0 0)
            eventEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 10 0 0 0)
            event = GCalEvent (Just "event") Nothing eventStart eventEnd

        gapRel gap event `shouldBe` IntersectGap

    context "with a post gap and equal start" $ do
      it "is an IntersectGap" $ do
        let baseDate = Date 01 January 1999
            gapStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 7 0 0 0)
            gapEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 12 0 0 0)
            gap = Period gapStart gapEnd Nothing
            eventStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 7 0 0 0)
            eventEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 10 0 0 0)
            event = GCalEvent (Just "event") Nothing eventStart eventEnd

        gapRel gap event `shouldBe` IntersectGap

  describe "splitGap" $ do
    it "splits a gap by a (strict-subset) event" $ do
      let baseDate = Date 01 January 1999
          gapStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 1 0 0 0)
          gapEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 12 0 0 0)
          gap = Period gapStart gapEnd Nothing
          eventStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 5 0 0 0)
          eventEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 10 0 0 0)
          event = GCalEvent (Just "event") Nothing eventStart eventEnd

          expectedFirst = Period gapStart eventStart Nothing
          expectedSecond = Period eventEnd gapEnd Nothing

      splitGap gap event `shouldBe` [expectedFirst, expectedSecond]

  describe "alterGaps" $ do
    context "with an event that's in-between gaps" $ do
      it "doesn't alter the gaps" $ do
        let baseDate = Date { dateDay = 01, dateMonth = January, dateYear = 1999 }
            gap1Start = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 1 0 0 0)
            gap1End = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 3 0 0 0)
            gap1 = Period gap1Start gap1End (Just "gap 1")
            gap2Start = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 9 0 0 0)
            gap2End = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 12 0 0 0)
            gap2 = Period gap2Start gap2End (Just "gap 2")

            eventStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 4 0 0 0)
            eventEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 8 0 0 0)
            event = GCalEvent (Just "event") Nothing eventStart eventEnd

        alterGaps event [gap1, gap2] `shouldBe` [gap1, gap2]

    context "with an event that covers one of the gaps" $ do
      it "removes that gap" $ do
        let baseDate = Date 01 January 1999
            gap1Start = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 1 0 0 0)
            gap1End = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 3 0 0 0)
            gap1 = Period gap1Start gap1End (Just "gap 1")
            gap2Start = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 9 0 0 0)
            gap2End = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 12 0 0 0)
            gap2 = Period gap2Start gap2End (Just "gap 2")

            eventStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 9 0 0 0)
            eventEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 14 0 0 0)
            event = GCalEvent (Just "event") Nothing eventStart eventEnd

        alterGaps event [gap1, gap2] `shouldBe` [gap1]

    context "with an event that partially covers one of the gaps" $ do
      it "removes that gap" $ do
        let baseDate = Date { dateDay = 01, dateMonth = January, dateYear = 1999 }

            gap1Start = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 1 0 0 0)
            gap1End = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 3 0 0 0)
            gap1 = Period gap1Start gap1End (Just "gap 1")

            gap2Start = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 9 0 0 0)
            gap2End = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 12 0 0 0)
            gap2 = Period gap2Start gap2End (Just "gap 2")

            eventStart = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 10 20 0 0)
            eventEnd = internTimeFromLocalOffset $ DateTime baseDate (TimeOfDay 13 0 0 0)
            event = GCalEvent (Just "event") Nothing eventStart eventEnd

        alterGaps event [gap1, gap2] `shouldBe` [gap1, Period gap2Start eventStart Nothing]

  describe "getAllGapsInRange" $ do
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
                    , periodStart = shiftTimeWZone (TimeOfDay (Hours 10) (Minutes 30) (Seconds 0) (NanoSeconds 0))
                    , periodEnd = shiftTimeWZone (TimeOfDay (Hours 16) (Minutes 00) (Seconds 0) (NanoSeconds 0))
                    }
                ]
              }

            sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }

            event1Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 10 30 0 0)
            event1End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 12 0 0 0)
            event1 = GCalEvent (Just "event 1") Nothing event1Start event1End

            event2Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 12 0 0 0)
            event2End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 14 0 0 0)
            event2 = GCalEvent (Just "event 2") Nothing event2Start event2End

            events = [event1, event2]

            gapStart = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 14 0 0 0)
            gapEnd = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 16 0 0 0)
            expectedGap = Period gapStart gapEnd Nothing

            dateRange = ( Start . internTimeFromLocalOffset $ sunday
                        , End . internTimeFromLocalOffset . eod $ sunday
                        )

            results = getAllGapsInRange localOffset shifts dateRange events

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
                    , periodStart = shiftTimeWZone (TimeOfDay (Hours 10) (Minutes 30) (Seconds 0) (NanoSeconds 0))
                    , periodEnd = shiftTimeWZone (TimeOfDay (Hours 16) (Minutes 00) (Seconds 0) (NanoSeconds 0))
                    }
                ]
              , wkSun =
                [
                  Period {
                    periodName = Nothing
                    , periodStart = shiftTimeWZone (TimeOfDay (Hours 10) (Minutes 30) (Seconds 0) (NanoSeconds 0))
                    , periodEnd = shiftTimeWZone (TimeOfDay (Hours 16) (Minutes 00) (Seconds 0) (NanoSeconds 0))
                    }
                ]
              }

          sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }

          event1Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 10 30 0 0)
          event1End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 12 0 0 0)
          event1 = GCalEvent (Just "event 1") Nothing event1Start event1End

          event2Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 12 0 0 0)
          event2End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 14 0 0 0)
          event2 = GCalEvent (Just "event 2") Nothing event2Start event2End

          sundayEvents = [event1, event2]

          gap1Start = internTimeFromLocalOffset $ DateTime friday (TimeOfDay 14 0 0 0)
          gap1End = internTimeFromLocalOffset $ DateTime friday (TimeOfDay 16 0 0 0)
          expectedGap1 = Period gap1Start gap1End Nothing

          friday = Date { dateDay = 02, dateMonth = March, dateYear = 2018 }

          event3Start = internTimeFromLocalOffset $ DateTime friday (TimeOfDay 10 30 0 0)
          event3End = internTimeFromLocalOffset $ DateTime friday (TimeOfDay 12 0 0 0)
          event3 = GCalEvent (Just "event 3") Nothing event3Start event3End

          event4Start = internTimeFromLocalOffset $ DateTime friday (TimeOfDay 12 0 0 0)
          event4End = internTimeFromLocalOffset $ DateTime friday (TimeOfDay 14 0 0 0)
          event4 = GCalEvent (Just "event 4") Nothing event4Start event4End

          fridayEvents = [event3, event4]

          gap2Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 14 0 0 0)
          gap2End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 16 0 0 0)
          expectedGap2 = Period gap2Start gap2End Nothing

          dateRange = ( Start . internTimeFromLocalOffset $ friday
                      , End . internTimeFromLocalOffset . eod $ sunday
                      )

          results = getAllGapsInRange localOffset shifts dateRange (fridayEvents ++ sundayEvents)

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
                    , periodStart = shiftTimeWZone (TimeOfDay (Hours 10) (Minutes 30) (Seconds 0) (NanoSeconds 0))
                    , periodEnd = shiftTimeWZone (TimeOfDay (Hours 16) (Minutes 00) (Seconds 0) (NanoSeconds 0))
                    }
                ]
              }

          sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }

          event1Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 10 30 0 0)
          event1End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 12 0 0 0)
          event1 = GCalEvent (Just "event 1") Nothing event1Start event1End

          event2Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 12 0 0 0)
          event2End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 16 0 0 0)
          event2 = GCalEvent (Just "event 2") Nothing event2Start event2End

          sundayEvents = [event1, event2]
          dateRange = (
              Start . internTimeFromLocalOffset $ Date { dateDay = 02, dateMonth = March, dateYear = 2018 }
            , End . internTimeFromLocalOffset $  Date { dateDay = 06, dateMonth = March, dateYear = 2018 }
            )

          results = getAllGapsInRange localOffset shifts dateRange sundayEvents

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

          event1Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 10 30 0 0)
          event1End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 12 0 0 0)
          event1 = GCalEvent (Just "event 1") Nothing event1Start event1End

          sundayEvents = [ event1 ]

          dateRange = (
              Start . internTimeFromLocalOffset $ Date { dateDay = 01, dateMonth = March, dateYear = 2018 }
            , End . internTimeFromLocalOffset $ Date { dateDay = 10, dateMonth = March, dateYear = 2018 }
                      )
          results = getAllGapsInRange localOffset shifts dateRange sundayEvents

        results `shouldBe` Gaps []

    context "with no events" $ do
      it "turns schedule shifts into gaps" $ do
        let
          sundayShift = Period
            {
              periodName = Just "sunday shift"
            , periodStart = shiftTimeWZone (TimeOfDay (Hours 10) (Minutes 30) (Seconds 0) (NanoSeconds 0))
            , periodEnd = shiftTimeWZone (TimeOfDay (Hours 16) (Minutes 00) (Seconds 0) (NanoSeconds 0))
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
          gapStart = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 10 30 0 0)
          gapEnd = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 16 0 0 0)
          expectedGap = Period gapStart gapEnd $ Just "sunday shift"

          start = Start . internTimeFromLocalOffset $ sunday
          end = End . internTimeFromLocalOffset . eod $ sunday

          dateRange = (start, end)

          results = getAllGapsInRange localOffset shifts dateRange []

        results `shouldBe` Gaps [expectedGap]

  describe "getMinGapsInRange" $ do
    context "with a gap below the miniumum and a gap above" $ do
      it "finds the gap above the minumum" $ do
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
                    periodName = Just "small gap"
                    , periodStart = shiftTimeWZone (TimeOfDay (Hours 10) (Minutes 30) (Seconds 0) (NanoSeconds 0))
                    , periodEnd = shiftTimeWZone (TimeOfDay (Hours 11) (Minutes 0) (Seconds 0) (NanoSeconds 0))
                    }
                ]
              , wkSun =
                [
                  Period {
                    periodName = Just "large gap"
                    , periodStart = shiftTimeWZone (TimeOfDay (Hours 10) (Minutes 30) (Seconds 0) (NanoSeconds 0))
                    , periodEnd = shiftTimeWZone (TimeOfDay (Hours 16) (Minutes 00) (Seconds 0) (NanoSeconds 0))
                    }
                ]
              }

          sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }
          friday = Date { dateDay = 02, dateMonth = March, dateYear = 2018 }

          gap2Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 10 30 0 0)
          gap2End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 16 0 0 0)
          expectedGap2 = Period gap2Start gap2End $ Just "large gap"

          dateRange = ( Start . internTimeFromLocalOffset $ friday
                      , End . internTimeFromLocalOffset . eod $ sunday
                      )

          minMinutes = 60

          results = getMinGapsInRange localOffset minMinutes shifts dateRange []

        results `shouldBe` Gaps [expectedGap2]

  describe "shiftsInrange" $ do
    it "selects all shifts that start in the range" $ do
      let firstExpected = Period {
            periodName = Just "first"
            , periodStart = shiftTimeWZone (TimeOfDay (Hours 16) (Minutes 0) (Seconds 0) (NanoSeconds 0))
            , periodEnd = shiftTimeWZone (TimeOfDay (Hours 21) (Minutes 00) (Seconds 0) (NanoSeconds 0))
            }
          secondExpected = Period {
            periodName = Just "second"
            , periodStart = shiftTimeWZone (TimeOfDay (Hours 10) (Minutes 30) (Seconds 0) (NanoSeconds 0))
            , periodEnd = shiftTimeWZone (TimeOfDay (Hours 16) (Minutes 00) (Seconds 0) (NanoSeconds 0))
            }
          shifts = ShiftW
              {
                wkMon = []
              , wkTue =
                  [
                    secondExpected
                  , Period {
                    periodName = Just "after"
                    , periodStart = shiftTimeWZone (TimeOfDay (Hours 16) (Minutes 0) (Seconds 0) (NanoSeconds 0))
                    , periodEnd = shiftTimeWZone (TimeOfDay (Hours 21) (Minutes 00) (Seconds 0) (NanoSeconds 0))
                    }
                ]
              , wkWed = []
              , wkThu = []
              , wkFri = []
              , wkSat = []
              , wkSun =
                [
                  Period {
                    periodName = Just "prior"
                    , periodStart = shiftTimeWZone (TimeOfDay (Hours 10) (Minutes 30) (Seconds 0) (NanoSeconds 0))
                    , periodEnd = shiftTimeWZone (TimeOfDay (Hours 16) (Minutes 00) (Seconds 0) (NanoSeconds 0))
                    }
                , firstExpected
                ]
              }

          sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }
          tuesday = Date { dateDay = 06, dateMonth = March, dateYear = 2018 }

          startDate = DateTime sunday $ TimeOfDay 12 0 0 0
          endDate = DateTime tuesday $ TimeOfDay 13 0 0 0

          start = Start . internTimeFromLocalOffset $ startDate
          end = End . internTimeFromLocalOffset $ endDate

          expectedShifts = [ (sunday, [intoIntern sunday firstExpected])
                           , (tuesday, [intoIntern tuesday secondExpected])
                           ]

      shiftsInRange localOffset (start, end) shifts `shouldBe` expectedShifts


  describe "eventDuration" $ do
    it "calculates the duration of the period" $ do
      let
        sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }
        eventStart = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 9 30 0 0)
        eventEnd = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 12 0 0 0)
        event = GCalEvent (Just "event") Nothing eventStart eventEnd

      eventDuration event `shouldBe` (Minutes $ (2 * 60) + 30, Seconds 0)

  describe "totalDuration" $ do
    it "sums durations of a collection of events" $ do
      let
        sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }
        event1Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 9 30 0 0)
        event1End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 12 0 0 0)
        event1 = GCalEvent (Just "event1") Nothing event1Start event1End

        event2Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 14 30 0 0)
        event2End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 16 30 0 0)
        event2 = GCalEvent (Just "event2") Nothing event2Start event2End

      totalDuration [event1, event2] `shouldBe` (Minutes $ (4 * 60) + 30, Seconds 0)

  describe "eventBySummDuration" $ do
    it "groups by event summary, and sums hours/minutes" $ do
      let
        sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }

        xEvent1Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 9 30 0 0)
        xEvent1End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 12 0 0 0)
        xEvent1 = GCalEvent (Just "x") Nothing xEvent1Start xEvent1End

        yEventStart = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 14 30 0 0)
        yEventEnd = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 16 45 0 0)
        yEvent = GCalEvent (Just "y") Nothing yEventStart yEventEnd

        xEvent2Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 9 30 0 0)
        xEvent2End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 12 0 0 0)
        xEvent2 = GCalEvent (Just " X  ") Nothing xEvent2Start xEvent2End

        expected = [ ("x", Hours 5, Minutes 0)
                   , ("y", Hours 2, Minutes 15)
                   ]

      eventBySummDuration [xEvent1, yEvent, xEvent2] `shouldBe` expected

  describe "weekOf" $ do
    it "finds the week of the current date" $ do
      let
        sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }

        start = Start $ Date { dateDay = 28, dateMonth = February, dateYear = 2018 }
        end = End $ Date { dateDay = 6, dateMonth = March, dateYear = 2018 }

      weekOf sunday `shouldBe` (start, end)

  describe "stafferOnCal" $ do
    context "when staffer's name is in the event summary" $ do
      it "is true" $ do
        let
          name = "Suze Z"

          sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }

          event1Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 9 30 0 0)
          event1End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 12 0 0 0)
          event1 = GCalEvent (Just "suze c") Nothing event1Start event1End

          event2Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 14 30 0 0)
          event2End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 16 30 0 0)
          event2 = GCalEvent (Just "well suZe z has fun!") Nothing event2Start event2End

        stafferOnCal name [event1, event2] `shouldBe` True

    context "when staffer's name is NOT in the event summary" $ do
      it "is false" $ do
        let
          name = "Suze Z"

          sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }

          event1Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 9 30 0 0)
          event1End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 12 0 0 0)
          event1 = GCalEvent (Just "suze c") Nothing event1Start event1End

          event2Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 14 30 0 0)
          event2End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 16 30 0 0)
          event2 = GCalEvent (Just "well suZe k has fun!") Nothing event2Start event2End

        stafferOnCal name [event1, event2] `shouldBe` False

  describe "hasOverlap" $ do
    context "when a new event has overlaps with existing events" $ do
      it "is true" $ do
        let sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }

            prevEventStart = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 10 0 0 0)
            prevEventEnd = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 12 0 0 0)
            prevEvent = GCalEvent (Just "") Nothing prevEventStart prevEventEnd

            prevEvent2Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 14 0 0 0)
            prevEvent2End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 16 0 0 0)
            prevEvent2 = GCalEvent (Just "") Nothing prevEvent2Start prevEvent2End

            newEventStart = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 15 30 0 0)
            newEventEnd = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 18 00 0 0)
            newEvent = GCalEvent (Just "") Nothing newEventStart newEventEnd

        hasOverlap [prevEvent, prevEvent2] newEvent `shouldBe` True

    context "when a new event has no overlaps with existing events" $ do
      it "is false" $ do
        let sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }

            prevEventStart = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 10 0 0 0)
            prevEventEnd = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 12 0 0 0)
            prevEvent = GCalEvent (Just "") Nothing prevEventStart prevEventEnd

            newEventStart = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 15 30 0 0)
            newEventEnd = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 18 00 0 0)
            newEvent = GCalEvent (Just "") Nothing newEventStart newEventEnd

        hasOverlap [prevEvent] newEvent `shouldBe` False

  describe "onlyActive" $ do
    context "with active an inactive contacts" $ do
      it "filters out inactive contacts" $ do
        let
          nowDate = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }
          past = getInternTime utcOffset $ advanceDate (Days (-3)) nowDate
          future = getInternTime utcOffset $ advanceDate (Days 3) nowDate
          now = getInternTime utcOffset $ nowDate

          active1 = Contact "Jez" "1561351" Nothing 1 Set.empty
          active2 = Contact "Big Suze" "156131" (Just past) 2 Set.empty
          inactive = Contact "Mark" "8946531" (Just future) 3 Set.empty

        onlyActive now [active1, inactive, active2] `shouldBe` [Active active1, Active active2]

    context "with only inactive contacts" $ do
      it "is empty" $ do
        let
          nowDate = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }
          future = getInternTime utcOffset $ advanceDate (Days 3) nowDate
          distantFuture = getInternTime utcOffset $ advanceDate (Days 30) nowDate
          now = getInternTime utcOffset $ nowDate

          inactive1 = Contact "Big Suze" "156131" (Just future) 2 Set.empty
          inactive2 = Contact "Mark" "8946531" (Just distantFuture) 3 Set.empty

        onlyActive now [inactive1, inactive2] `shouldBe` []

  describe "displaying gaps" $ do
    it "shows correctly formatted times" $ do
      let sun = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }
          sunStart = DisplayTZ $ localTime localOffset $ timeGetElapsed $ DateTime sun $ TimeOfDay 6 0 0 0
          sunEnd = DisplayTZ $ localTime localOffset $ timeGetElapsed $ DateTime sun $ TimeOfDay 23 15 0 0

          mon = Date { dateDay = 05, dateMonth = March, dateYear = 2018 }
          monStart = DisplayTZ $ localTime localOffset $ timeGetElapsed $ DateTime mon $ TimeOfDay 12 50 0 0
          monEnd = DisplayTZ $ localTime localOffset $ timeGetElapsed $ DateTime mon $ TimeOfDay 18 15 0 0

          gaps = Gaps [ Period sunStart sunEnd Nothing
                      , Period monStart monEnd Nothing
                      ]

          expectedOutput = "Open shifts: \n[Sun 4 6AM to 11:15PM\n, Mon 5 12:50PM to 6:15PM]\nRespond with \"shifts\" to claim one right now"
      displayGaps gaps `shouldBe` expectedOutput

  describe "selecting which staffers to nag" $ do
    it "only selects staffers who don't have a shift yet" $ do
      let sunday = Date { dateDay = 04, dateMonth = March, dateYear = 2018 }
          monday = Date { dateDay = 05, dateMonth = March, dateYear = 2018 }

          event1Start = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 15 30 0 0)
          event1End = internTimeFromLocalOffset $ DateTime sunday (TimeOfDay 18 00 0 0)
          event1 = GCalEvent (Just "Jeffers") Nothing event1Start event1End

          event2Start = internTimeFromLocalOffset $ DateTime monday (TimeOfDay 15 30 0 0)
          event2End = internTimeFromLocalOffset $ DateTime monday (TimeOfDay 18 00 0 0)
          event2 = GCalEvent (Just "  beLINda ") Nothing event2Start event2End

          onSched = Active $ Contact "Belinda" "156131" Nothing 1 Set.empty
          unScheduled = Active $ Contact "Big Suze" "156131" Nothing 8 Set.empty

      whomToNag [event1, event2] [onSched, unScheduled] `shouldBe` [unScheduled]

  describe "daysUntil" $ do
    context "3 days to target weekday" $ do
      it "evalutes to 3" $ do
        let sunday = getInternTime utcOffset $ Date { dateDay = 04, dateMonth = March, dateYear = 2018 }
        daysUntil utcOffset sunday Wednesday `shouldBe` Days 3

    context "on target weekday" $ do
      it "evalutes to zero" $ do
        let sunday = getInternTime utcOffset $ Date { dateDay = 04, dateMonth = March, dateYear = 2018 }
        daysUntil utcOffset sunday Sunday `shouldBe` Days 0

-- helpers

eod :: Date -> DateTime
eod d = DateTime d $ TimeOfDay 23 59 0 0

intoIntern :: Functor f => Date -> f (ShiftTime TimeOfDay) -> f InternTime
intoIntern day = fmap $ internTimeFromLocalOffset . DateTime day . shiftTimeTime

shiftTimeWZone :: a -> ShiftTime a
shiftTimeWZone a = ShiftTime a localOffset

instance Eq Data.Yaml.Internal.ParseException where
  _ == _ = False
